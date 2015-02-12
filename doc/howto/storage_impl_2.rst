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
Haskell garbage collector is quite efficient, but this is still quite a bit of
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
