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

    append :: RowColumn          -> Int64 -> Int64 -> VU.Vector Bool -> VU.Vector Int64 -> RowColumn
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

    update :: RowColumn          -> Int64 -> Int64         -> Int64 -> [Maybe Bool] -> VU.Vector Int64 -> RowColumn
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
          new_present_indexes = updateablePresentToIndex new_present

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

On line 27 of the `update` function, we zip together the present vectors for the
previous row (old) and the updates (new). Then we zip those together with the
index of the column they represent.

Line 28 maps that list into a simpler list of indexes and CopyColumn values by
pattern matching over the various options. Finally, line 29 generates our
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
        convert_truth 0 0 VU.empty
      where
        convert_truth offset index output
          | offset < VU.length present =
              if   present ! offset
              then convert_truth (offset+1) (index+1) (VU.snoc output index)
              else convert_truth (offset+1)  index    (VU.snoc output  (-1))

          | otherwise = output


This function is designed to generate a lookup vector for existing row data. It
is fairly simple:

 #. For each element of the present vector, generate a lookup index for the
    columns vector.
 #. If the present element is "True", then write the current index into the
    vector, increment the index, and process the next element.
 #. If the present element is "False", write a -1 into the vector, and go to
    the next element.

This particular implementation is not entirely satisfying, but the existing
vector traversal mechanisms do not lend themselves to carrying the index state
to succeeding elements. An attempt was made to utilize the state monad here,
but the results seemed even less readable.

As a simple example, if we had the following present vector:

   [False, True]

we would end up with this lookup vector:

   [   -1,    0]
