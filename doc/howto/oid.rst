.. _oid-definition:

Object Identifier
=======================

An object identifier is a concrete thing, but can often be implied or at least
highly compressed.

In general, and oid refers to a logical location for storing one piece of data.
You can think of it as a key, but the key is never actually stored near the
value it represents.

Oids are not universally unique. They refer to one location in one column on
one node. Other associated or unassociated columns may have the same oid without
collision, and without it meaning anything.

An oid is broken up into two pieces, a 48-bit segment identifier, and a 16-bit
offset into that segment. This means that segments will not have more than
64k items each. This limits the maximum size of segment arrays.

.. note:

  This split is not cast in stone yet. I'm not sure what the right segment size
  ought to be. It may be that a 40/24 split is better, but for now we'll go
  with this. It seems like having segments of 16 million items is too large.
  That would mean that most raw integer columns would be 64mb. While this might
  be good for certain kinds of array processing operations, it would also be
  a problem for seeking into compressed segments.
