Mutable segments
=================

The first thing we are going to implement is the data column. This is the most
important part of the data storage engine, because it's where the data goes.
Ordinarily we might just allocate an array, and then shove data into it.
However, that won't work.

First, we are storing columns as sorted segments. That means that we can't just
append data to arrays. Also consider that sorting will destroy the row
mappings because columns will change position as new data is inserted. So how do
we deal with this problem?

Basically a segment has two phases of life. One is as an unsorted list, and the
other is as a sorted array. While a segment is filling up it is just an
unsorted string of data values. After a segment has filled, it is "frozen" into
an efficient, sorted array. Of course, the row ids need to be remapped, but that
is a one time cost associated with freezing a segment. We assume that it is more
than paid for by the advantages of searching a sorted array.

Another problem is that piecewise operations on mutable arrays in Haskell can
be very expensive. It is much better to batch all of the operations up and, do
them once.

Consequently, the very first thing we have to do is build a primordial storage
area for segments. It needs to provide simple append operations, null
compression, and a way to get the data back.

This code is implemented as `src/Store/DataColumn.hs`. The full implementation
is included below.

.. note::

   The author is new to Haskell. Consequently, the initial exposition will be
   very verbose. Things which might seem obvious to a seasoned Haskell
   programmer might be explained in detail. This should be useful to others
   reading this documentation, who are also not familiar with Haskell.

Basic Setup
-------------

This is pretty simple so far. Haskell has a variety of integer types, but we
know that we want to use a few specifically sized variants because these things
are going to go into fixed sized fields on disk eventually.

.. code-block:: haskell
   :linenos:

   module DataColumn where

   import Data.Int (Int64, Int16)


The Types
-----------

We implement two types. We may have to rename them at some point, but this is
a good starting place. Note that we use lists instead of arrays because they
should be easier on the garbage collector. Additionally, storing strings in
a list is easier than storing them in an array. We will have to make special
arrangements to store string data.

.. note::

  The list stores boxed values, so an in-progress segment is going to be around
  8 times larger than even an uncompressed array. (Apparently this accounts for
  the boxing data and the pointers in the list. I couldn't find the original
  reference for this data.)

  Also, lists are O(n) for indexing. Keeping the primordial storage area small
  has a lot of benefits.

The Segment type is defined with a type parameter 'a', which means that we can
use it with any type we like. This is very similar to using a template in C++,
or generics with other languages.

.. code-block:: haskell
   :linenos:

    data Extent = Extent {
        start   :: Int64,
        len     :: Int16
    } deriving(Eq, Show)

    data Segment a = Segment {
        extents    :: [Extent],
        array      :: [a]
    }


Extents
---------

We need a way to manage extents. They are encoded as a range
of lists. Consecutive values extend the range. So first, let's implement a
way to append an oid to the extent list.

.. code-block:: haskell
   :linenos:

   appendExtent :: Int64 -> [Extent] -> [Extent]
   appendExtent    oid        []      = [Extent {start=oid, len=0}]
   appendExtent    oid        xts     =
       if oid - (xt_start + fromIntegral xt_length) == 1
       then Extent {start=xt_start, len=xt_length+1} : the_rest
       else Extent {start=oid, len=0}      : current : the_rest
       where
           (current:the_rest) = xts
           Extent {start=xt_start, len=xt_length} = current

This is a very simple function. We take an oid and the existing list of extents.
We split the extents into a head and tail in line 8. Those are used to see if
the current oid is consecutive with the previous. If so we return a new extent
that covers the whole range, and join it with the previous list. Otherwise we
create a new extent starting with this new oid, then append the previous head
and the tail.

We can see this works:

.. code-block:: hs

   Prelude> :l DataColumn.hs
   [1 of 1] Compiling DataColumn       ( DataColumn.hs, interpreted )
   Ok, modules loaded: DataColumn.
   *DataColumn> appendExtent 1 []
   [Extent {start = 1, len = 0}]


Simple calls work well, but what about big ones? To make that easier, we build
a new function:

.. code-block:: haskell
   :linenos:

   extentFromList :: [Extent] -> [ Int64 ] -> [Extent]
   extentFromList       xt       [       ] =  xt
   extentFromList       xt       ( x:xs  ) =
      extentFromList r xs
      where
         r = appendExtent xt x


Now we try it:

.. code-block:: hs

   *DataColumn> extentFromList [] [1..100000]
   [Extent {start = 98308, len = 1692},Extent {start = 65539, len = -32768},Extent {start = 32770, len = -32768},Extent {start = 1, len = -32768}]


We missed a boundary condition. We have to make sure the length of any one extent
doesn't exceed the size of Int16. To do that we change line 4 above:

.. code-block:: hs
   :linenos:

    if    oid - (xt_start + fromIntegral xt_length) == 1
       && xt_length < (maxBound :: Int16)

Which works much better:

.. code-block:: hs

   *DataColumn Data.Int> :l DataColumn.hs
   [1 of 1] Compiling DataColumn       ( DataColumn.hs, interpreted )
   Ok, modules loaded: DataColumn.
   *DataColumn Data.Int> extentFromList [] [1..100000]
   [Extent {start = 98305, len = 1695},Extent {start = 65537, len = 32767},Extent {start = 32769, len = 32767},Extent {start = 1, len = 32767}]

   *DataColumn Data.Int> let a = extentFromList [] [1..100000]
   *DataColumn Data.Int> extentFromList a [200000..300000]
   [Extent {start = 298304, len = 1696},Extent {start = 265536, len = 32767},Extent {start = 232768, len = 32767},Extent {start = 200000, len = 32767},Extent {start = 98305, len = 1695},Extent {start = 65537, len = 32767},Extent {start = 32769, len = 32767},Extent {start = 1, len = 32767}]


Segments
---------

At this point we just need to be able to append a value to a segement. This is
actually quite simple:

.. code-block:: haskell
   :linenos:

   appendValue :: Segment a -> Int64 -> a -> Segment a
   appendValue Segment{extents=xt, array=arr} oid value =
     Segment{extents=appendExtent xt oid, array=value : arr}


Does it work?

.. code-block:: haskell
   :linenos:


   *DataColumn Data.Int> let s = Segment{extents=[], array=[]}
   *DataColumn Data.Int> s
   Segment {extents = [], array = []}
   *DataColumn Data.Int> appendValue s 1 100
   Segment {extents = [Extent {start = 1, len = 0}], array = [100]}

Indeed it does. Let's implement another helper for Segment like we did for Extent
and see if it works for larger values.

.. code-block:: haskell
   :linenos:

   segmentFromList :: Segment a -> [(Int64, a)] -> Segment a
   segmentFromList          seg    [          ] =  seg
   segmentFromList          seg    (   x:xs   ) =
      segmentFromList r xs
      where
         (oid, value) = x
         r = appendValue seg oid value


Now we try it...

.. code-block:: haskell

   *DataColumn Data.Int> :l DataColumn.hs
   [1 of 1] Compiling DataColumn       ( DataColumn.hs, interpreted )
   Ok, modules loaded: DataColumn.
   *DataColumn Data.Int> let s = Segment{extents=[], array=[]}
   *DataColumn Data.Int> segmentFromList s [(x, x*100) | x <- [1..100]]
   Segment {extents = [Extent {start = 1, len = 99}], array = [10000,9900,9800,9700,9600,9500,9400,9300,9200,9100,9000,8900,8800,8700,8600,8500,8400,8300,8200,8100,8000,7900,7800,7700,7600,7500,7400,7300,7200,7100,7000,6900,6800,6700,6600,6500,6400,6300,6200,6100,6000,5900,5800,5700,5600,5500,5400,5300,5200,5100,5000,4900,4800,4700,4600,4500,4400,4300,4200,4100,4000,3900,3800,3700,3600,3500,3400,3300,3200,3100,3000,2900,2800,2700,2600,2500,2400,2300,2200,2100,2000,1900,1800,1700,1600,1500,1400,1300,1200,1100,1000,900,800,700,600,500,400,300,200,100]}


Good enough for now.

.. note::

   Ideally we would write unit tests for this stuff. Haskell has a very
   sophisticated test framework called QuickCheck. Sadly, the author has not
   yet learned how to use it. That will be the subject of a future section in
   this documentation.

Source
---------

The entire source is included below:

.. literalinclude:: ../../src/Store/DataColumn.hs
  :language: haskell
  :linenos:
