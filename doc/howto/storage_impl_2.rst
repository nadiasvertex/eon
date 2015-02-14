Immutable segments
===================

These are the backbone of the storage system. First we will work on segments
which live in memory, later we will work on segments that get persisted to
disk.

The mutable segments we have made are easy to use, but they are not particularly
efficient. Note this execution of just 100 appends:

.. code-block:: hs

  *DataColumn> segmentFromList ps [(x,x*3) | x <- [1..100]]
  Segment {extents = [Extent {start = 1, len = 99}], array = [300,297,294,291,288,285,282,279,276,273,270,267,264,261,258,255,252,249,246,243,240,237,234,231,228,225,222,219,216,213,210,207,204,201,198,195,192,189,186,183,180,177,174,171,168,165,162,159,156,153,150,147,144,141,138,135,132,129,126,123,120,117,114,111,108,105,102,99,96,93,90,87,84,81,78,75,72,69,66,63,60,57,54,51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3]}
  (0.00 secs, 2577184 bytes)

On this same machine, appending 1,000 elements goes through 8mb of RAM. The
Haskell garbage collector is quite efficient, but this is still a lot of
memory. Of course, not all of this memory ends up allocated, its just that we
are allocating new structures frequently. Many of those don't survive. Still, we
want to store as much data as possible in the memory we have. That means being
very compact. And *that* means using arrays.

Arrays
----------

Haskell has several different array facilities. Unlike many other languages,
arrays are not really a common or basic facility. The
`array <https://wiki.haskell.org/Arrays>`_ page on the Haskell wiki is a good
place to start. After looking at the alternatives, we've decided to go with
the `repa package <https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial>`_.
It appears to be the premier array package. There is some additional
complexity involved in using it, due to its flexibility. However, it's not too
bad, and there are also high-level simplifications in the package that may make
our lives easier in other places.

Frozen Data
------------

We need a place to put our frozen data:

.. code-block:: haskell
   :linenos:

   data FrozenSegment a = FrozenSegment {
       column :: Array U DIM1 a
   } deriving(Eq, Show)

Some explanation may be in order. repa has a very rich type language for talking
about arrays. Without getting into that too much, we want an unboxed, one
dimensional array that contains items of type 'a'.

We could have just used a type alias for this, since it's so simple, but we will
almost certainly want to add more fields to the frozen segment (like min/max,
etc), so we'll go ahead and make it a user defined type.

We've also lost information about the oids. That's fine because we do want to
discard that overhead, but there's a transitional period where we'll still need
it. So we record that here:

.. code-block:: haskell
   :linenos:

   data SegmentMap = SegmentMap {
     extents :: [MD.Extent],
     indexes :: Array U DIM1 Int64
   } deriving (Eq, Show)

The extents field is the same as the one from the Segment object we saw earlier.
The indexes field is a list of oids in the same array slot as the values in the
"column" field of the frozen segment, only reversed. This is so that they match
the extents.

It is very likely that we will not need the extents field, so we may later
remove it. For now we keep it around because it is easy.

Finally, we write the freeze function:

.. code-block:: haskell
   :linenos:

   freeze :: (Ord a, Unbox a) => MD.Segment a -> (SegmentMap, FrozenSegment a)
   freeze                               seg    =
     (SegmentMap    {extents=xt, indexes=frozen_indexes},
      FrozenSegment {column=frozen_array})
     where
       size           = length arr
       frozen_array   = fromListUnboxed (ix1 size) values
       frozen_indexes = fromListUnboxed (ix1 size) (reverse oids)
       (values, oids) = unzip sorted
       sorted         = sortBy (comparing fst) (MD.enumerateSegment seg)
       MD.Segment{MD.array=arr, MD.extents=xt} = seg

This is pretty straightforward, although it was a little troublesome to get the
signature right. The function has to put restrictions on values of type 'a' so
that it can use `sortBy` and `fromListUnboxed`. If you don't you will get
strange errors about not having the right context.

Basically we unpack the segment, which creates a list of tuples (value, oid).
Then we sort that list by comparing the first item in each tuple (the value).
Finally, we unzip that list and pack the values and oids into two arrays.

Of course, Haskell being lazy, nothing really happens all at once. The values
stream through these transforms on request. We may need to do something about
that later in order to avoid space leaks. However, it may be a performance
feature, so we'll have to look at that closely when we get there.

We also fixed an off by one error in enumerateExtents that was flushed out by
testing this function.

The results:

.. code-block:: hs

  *Store.FrozenDataColumn Data.Array.Repa Data.List Data.Ord> :l Store/FrozenDataColumn.hs
  [1 of 2] Compiling Store.DataColumn ( Store/DataColumn.hs, interpreted )
  [2 of 2] Compiling Store.FrozenDataColumn ( Store/FrozenDataColumn.hs, interpreted )
  Ok, modules loaded: Store.DataColumn, Store.FrozenDataColumn.
  (0.04 secs, 46036936 bytes)
  *Store.FrozenDataColumn Data.Array.Repa Data.List Data.Ord> let s=MD.Segment{MD.array=[], MD.extents=[]}
  (0.00 secs, 1540808 bytes)
  *Store.FrozenDataColumn Data.Array.Repa Data.List Data.Ord> let s' = MD.segmentFromList s [(x,1000+x*3) | x <- [1..100]]
  (0.00 secs, 1545416 bytes)
  *Store.FrozenDataColumn Data.Array.Repa Data.List Data.Ord> freeze s'
  (SegmentMap {extents = [Extent {start = 1, len = 99}], indexes = AUnboxed (Z :. 100) (fromList [100,99,98,97,96,95,94,93,92,91,90,89,88,87,86,85,84,83,82,81,80,79,78,77,76,75,74,73,72,71,70,69,68,67,66,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1])},FrozenSegment {column = AUnboxed (Z :. 100) (fromList [1003,1006,1009,1012,1015,1018,1021,1024,1027,1030,1033,1036,1039,1042,1045,1048,1051,1054,1057,1060,1063,1066,1069,1072,1075,1078,1081,1084,1087,1090,1093,1096,1099,1102,1105,1108,1111,1114,1117,1120,1123,1126,1129,1132,1135,1138,1141,1144,1147,1150,1153,1156,1159,1162,1165,1168,1171,1174,1177,1180,1183,1186,1189,1192,1195,1198,1201,1204,1207,1210,1213,1216,1219,1222,1225,1228,1231,1234,1237,1240,1243,1246,1249,1252,1255,1258,1261,1264,1267,1270,1273,1276,1279,1282,1285,1288,1291,1294,1297,1300])})
  (0.00 secs, 3129632 bytes)

Also, we discovered something else useful. The size statistic here also
includes memory involved in printing out the results. For example:

.. code-block:: hs

  *Store.FrozenDataColumn Data.Array.Repa Data.List Data.Ord> let e = freeze s'
  (0.00 secs, 551376 bytes)

This is much smaller than the the expression above where we print out the result
of `freeze s'`. So in the future, we'll use that value when looking at ballpark
memory usage in ghci.



Source
---------

The entire source is included below:

.. literalinclude:: ../../src/Store/FrozenDataColumn.hs
  :language: haskell
  :linenos:
