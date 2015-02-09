First steps
===============

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

.. code-block:: ghci

Prelude> :l DataColumn.hs
[1 of 1] Compiling DataColumn       ( DataColumn.hs, interpreted )
Ok, modules loaded: DataColumn.
*DataColumn> appendExtent 1 []
[Extent {start = 1, len = 0}]



The entire source is included below:

.. literalinclude:: ../../src/Store/DataColumn.hs
  :language: haskell
  :linenos:
