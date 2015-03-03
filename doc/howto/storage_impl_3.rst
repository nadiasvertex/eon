The Row Column
===================

This is one of the more intricate structures to implement. The row column is the
column which maintains the integrity of the row. It keeps a version, as well
as the addresses of the columns present, and a small structure indicating
which columns actually are present.

The basic per-row structure is actually simple:

.. code-block:: haskell
   :linenos:

   import qualified Data.Vector.Unboxed as VU

   data Row = Row {
    rid     :: Int64,           -- The row id.
    version :: Int64,           -- The version of this row.
    columns :: VU.Vector Int64, -- The addresses of the columns for this row.
    present :: VU.Vector Bool   -- Which columns are present in this row.
   } deriving (Show)


The `rid` and `version` fields should be obvious. They track the row id and the
row version. A row is never updated in place, instead we implement a copy on
write algorithm. When a row is updated we increment the version, and then
merge the previous row's values with the updated row values.

The `columns` field contains the oids of the column values that make up the
row. It is only as long as the number of actual columns present in this row.
Null values are omitted. The `present` field always contains a packed vector of
bools with one element in the vector for each column in the table. If the
element is True, the column is present in this row, False if the column is set
to null.

We need to quickly locate a particular row, so we store them in a map, indexed
by row id.

.. code-block:: haskell
   :linenos:

   data RowColumn = RowColumn {
     rows :: Map.Map Int64 [Row]
   } deriving (Show)

Of course, this declaration could be simpler, but there are other things that
we want to add here. We'll get to that later, though. For now it should be
enough.

.. note::

  You may have noticed that the value type in the map is a list of rows. This
  is important. Remember that each row may have multiple versions. We store
  each version of the row in the same map bucket, sorted in order of version.

Appending New Rows
-------------------

The append process for new rows is very simple. We take the data for the new
row and create a Row object. We find the row's entry in the map, and prepend
the new row to the list of rows already in that bucket.

.. code-block:: haskell
   :linenos:

    append ::
           RowColumn             -> Int64 -> Int64 -> VU.Vector Bool -> VU.Vector Int64 -> RowColumn
    append RowColumn{rows=old_rows} rid      version  present           values =
       RowColumn{rows=new_rows}
       where
          new_rows         = Map.insert rid new_row_versions old_rows
          old_row_versions = fromMaybe [] (Map.lookup rid old_rows)

          new_row_versions = new_row_version : old_row_versions
          new_row_version  = Row{
             rid    =rid,
             version=version,
             columns=values,
             present=present
          }


Updating Rows
-------------------

This step is a little more involved. Remember earlier we said that we never
update a row in place. This is good for Haskell, and for our database because
we use MVCC. Old row versions get pruned as soon as no outstanding transactions
refer to them. Since we don't have the transactional logic in place, we won't
worry about that for now.

A brief overview, before we get into the guts of the function. Basically, the
idea is to find the previous version of the row, merge its values with those
specified in the call to update, and then prepend the new row to the list of
rows in the map.

.. code-block:: haskell
   :linenos:

    update ::
           RowColumn             -> Int64 -> Int64         -> Int64 -> [Maybe Bool] -> VU.Vector Int64 -> RowColumn
    update RowColumn{rows=old_rows} rid      previous_version version  new_present     new_values =
       RowColumn{rows=new_rows}
       where
          new_rows = Map.insert rid new_row_versions old_rows

          match_row Row{version=current_version} =
             current_version == previous_version

          old_row_versions = fromMaybe [] (Map.lookup rid old_rows)
          old_row_version  = head $ filter match_row old_row_versions

          new_row_versions = new_row_version : old_row_versions
          new_row_version  = Row {
             rid    =rid,
             version=version,
             columns=final_values,
             present=updated_present
          }

          old_present_indexes = presentToIndex (present old_row_version)
          new_present_indexes = presentToIndexWithNull new_present

          old_values        = columns old_row_version
          old_present       = VU.toList $ present old_row_version
          updated_present   = VU.fromList $ map match_presence fused_presence
          combined_presence = zip [0..] $ zip new_present old_present
          fused_presence    = map map_presence combined_presence
          copy_program      = filter match_presence fused_presence

          -- Figure out where the final column value comes from.
          map_presence (ix, (Nothing,       _)) = (ix, None)
          map_presence (ix, (Just True,     _)) = (ix,  New)
          map_presence (ix, (Just False, True)) = (ix,  Old)

          -- Determine which columns can be ignored
          match_presence (_, None) = False
          match_presence (_,    _) = True

          -- Copy the column values from the right location
          copy_column (ix, Old) = old_values ! (old_present_indexes ! ix)
          copy_column (ix, New) = new_values ! (new_present_indexes ! ix)

          final_values  = VU.fromList $ reverse $ map copy_column copy_program

Most of the interior functions are extractions of various sorts. However, some
of the functions are subtle and important, so we'll call those out.

Merging Row Values
~~~~~~~~~~~~~~~~~~~

One of the most subtle bits in this function has to do with merging the previous
row and the current row. The author had to rewrite this function several times
before getting it right.

 #. The old row may have some columns present, and other columns not.
 #. The update may add columns to the row, _or_ delete them (by setting their
    value to null.)
 #. We need to copy the right columns from both the old and new sets, and make
    sure that we drop any deleted columns.

Doing this correctly is not entirely trivial, so as an intermediate step we have
the function generate a copy "program." This is just a list of tuples that
contain the column index, and an operation.

.. code-block:: haskell
   :linenos:

    data CopyColumn = None | Old | New

On line 28 of the `update` function, we zip together the present vectors for the
previous row (old) and the updates (new). Then we zip those together with the
index of the column they represent.

Line 29 maps that list into a simpler list of indexes and CopyColumn values by
pattern matching over the various options. Finally, line 30 generates our
copy program by eliminating any 'None' operations from the list.

While the explanation given here has used procedural language, it is the
author's expectation that most of these operations will happen interleaved.
That is, the transforms will happen on-demand for each element, instead of
generating a number of different lists and transforming those one at a time.

Copying Final Column Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This part is also subtle. Remember that we store only non-null columns in each
row. This means that in a table with 5 columns, if a single row has only two
of those columns set to non-null values then the length of the columns vector
for that row will be 2, while the length of the present vector will be 5. Since
the "old" and "new" present vectors may differ, we need a way to map the
absolute column indexes to the actual column vectors for each row.

.. code-block:: haskell
   :linenos:

    presentToIndex :: VU.Vector Bool -> VU.Vector Int
    presentToIndex present  =
        VU.map mapper truth_indexes
      where
        accum acc el         = if el then acc+1 else acc
        mapper (present, ix) = if present then ix else -1
        truth_indexes        = VU.zip present indexes
        indexes              = VU.prescanl accum 0 present

This function is designed to generate a lookup vector for existing row data. It
is fairly simple and uses some vector-specific functions that should be very
efficient:

 #. (Line 8) Run an accumulator (Line 5) over the present vector that increases
    only when the presence element is True.
 #. (Line 5) If the present element is "True", then bump the accumulator,
    otherwise return the current value.
 #. (Line 7) Zip the indexes we just generated together with the present vector.
 #. (Line 3) Map the vector we made in the previous step to a new vector where
    every (False, _) tuple is replaced by -1, and every (True, ix) is replaced
    with ix.

As a simple example, if we had the following present vector:

   [False, True]

we would generate the following intermediate structure:

   [(False, 0), (True, 0)]

and end up with this lookup vector:

   [   -1,    0]

This allows us to lookup absolute column index 1, and see that it is found at
local column index 0. You can see the usage of this lookup table in lines 41
and 42 in "update" above.

A longer example:

[False,False,True,True,False,True]

intermediate:

[(False, 0),(False,0),(True, 0),(True, 1),(False, 1),(True,2)]

final:

[-1,-1,0,1,-1,2]

We wrote a similar, but slighty more complex function for the new_present values
passed into update. The additional complexity is due to the use of Maybe to
encode columns that might have been set to null in an update (and thus deleted
from the current row).

.. code-block:: haskell
   :linenos:

   presentToIndexWithNull :: [Maybe Bool] -> VU.Vector Int
   presentToIndexWithNull  =
     VU.fromList . snd . mapAccumL accum 0
     where
       accum acc el = if convert_present el then (acc+1, acc) else (acc, -1)

       convert_present Nothing      = False
       convert_present (Just False) = False
       convert_present (Just  True) = True

You can see that this is basically the same function, but rewritten to handle
the ternary logic needed for dealing with NULL. It's also written in a simpler
format using a list.

Generating Final Column Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The result of the update function basically derives from lines 45 and 27, where
we run the copy program and merge the old and new vectors into its final
output.

Line 6 provides an updated map with the new row inserted into the right place.

Lookups
---------

Finding a particular row version is very simple. It's just a matter of finding
the list of row versions in the row map, and then returning the first one that
matches the version. The versions are sorted, so we can early out. Since the
versions are stored in a list, it doesn't make sense to use a binary search to
find the right one. We could do early-out for better performance, but the number
of versions of a row should always be small enough that we don't really care
at this point. Perhaps it's an optimization we'll do later.

.. code-block:: haskell

  lookup row_column row_id row_version =
    do
      row_versions  <- Map.lookup row_id (rows row_column)
      last_version  <- listToMaybe . reverse $ filter match_version row_versions
      return last_version
    where
      match_version Row{version=current_version} = row_version == current_version


Unit Tests
---------------------

After spending some time researching various supported methods for writing
tests in Haskell, it was determined that the best choice would be a mix of
doc tests and spec tests, as explained in
`this tutorial. <https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md>`_

Briefly, the idea is that doc tests should explain how to use the library, and
spec tests should perform algorithmic testing. The spec tests can include
QuickCheck property tests too.

A sample run:

.. code-block:: bash

  $ cabal test
  Building eon-0.1.0.0...
  Preprocessing library eon-0.1.0.0...
  [1 of 1] Compiling Store.RowColumn  ( src/Store/RowColumn.hs, dist/build/Store/RowColumn.o )
  In-place registering eon-0.1.0.0...
  Preprocessing executable 'eon' for eon-0.1.0.0...
  [1 of 2] Compiling Store.RowColumn  ( src/Store/RowColumn.hs, dist/build/eon/eon-tmp/Store/RowColumn.o )
  Linking dist/build/eon/eon ...
  Preprocessing test suite 'doctest' for eon-0.1.0.0...
  Preprocessing test suite 'spec' for eon-0.1.0.0...
  Linking dist/build/spec/spec ...
  Running 2 test suites...
  Test suite doctest: RUNNING...
  Test suite doctest: PASS
  Test suite logged to: dist/test/eon-0.1.0.0-doctest.log
  Test suite spec: RUNNING...
  Test suite spec: PASS
  Test suite logged to: dist/test/eon-0.1.0.0-spec.log
  2 of 2 test suites (2 of 2 test cases) passed.


Source
---------

The entire source is included below:

.. literalinclude:: ../../src/Store/RowColumn.hs
  :language: haskell
  :linenos:

Tests
---------

The entire spec test is included below:

.. literalinclude:: ../../test/RowColumnSpec.hs
  :language: haskell
  :linenos:
