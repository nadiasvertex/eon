Storage Format
==========================================

We are going to start by defining the on-disk and in-memory format of data. This
is a key part of the design, and much of the data store's design and
implementation follows from these early choices.

Columnar Storage
-----------------

One early design decision is to provide columnar storage. A columnar format
allows for simpler and more efficient compression of data. It also allows
efficient operations on the data, since we can treat columns like large
arrays. Efficient array processing is a well-understood science, and there
are many software packages that provide high-performance array operations.

We would like to provide the following storage modes:

  #. Delta compression. For numeric formats, store only the differences
     between successive values. This can reduce data size significantly.
  #. Run length encoding. Some values may repeat many times. Instead of
     storing each value many times, store the value once, followed by a repeat
     count.
  #. Arity compression. Some columns have a small number of distinct values.
     Instead of storing the same value many times, store the distinct values
     once, and then store a small integer index into the list of distinct
     values. This mode may optionally provide run length encoding as well.
  #. Substituion compression. This is basically yielding to algorithms like
     lz4, deflate, bzip2, lzma, etc. For some kinds of data we cannot do
     better.

In addition, we would like to be able to use these different modes at the
same time in a single column. Consequently, we break a column up into segments.
Each segment has its own storage mode. That may be raw, or it may involve one
of the compressed modes above. This is true regardless of whether the data
is stored in memory or on disk.

In-memory Format
~~~~~~~~~~~~~~~~

The in-memory format is as compact as possible, without sacrificing performance.
Each segment is stored as two arrays with a few associated values. One
associated  value is the storage mode. We store the number of values that are in
the column, as well as the minimum and maximum values. We would also like to
store a bloom filter for optimal In addition we store how
many reads, insertions, updates, and deletes have occurred on this segment. This
will help us understand how important this segment is, and may affect its
candidacy for eviction or retention from memory.

The two arrays are important. One array stores every value written (either
directly, or in some compressed way.) The other stores a list of extents. The
extent is a run length encoding of the logically row address of the column.
Every column has an oid. The oid for each row is the same for every column. This
is how we combine columns into rows. One guarantee is that the oid is
monotonically increasing. This allows us to run length encode the oids for a
segment. Why isn't a single extent sufficient?

Basically because we want to have zero-size storage for null values. Instead of
storing the null value, we simply indicate a discontiguity in the column by
creating a new extent. There are certainly data sets where this will lead to
data expansion, so something may need to be done about this. (For example,
imagine we store a null and then a value, then a null, and a value. This would
create 50% more data than just storing an oid for each value.)

On-disk Format
~~~~~~~~~~~~~~~

The on-disk format is designed to conserve space. All integer values are written
in varint format. Each segment is stored in a separate file, with a small
header. The header contains the associated values, like the statistics mentioned.

Following the header, the data is stored directly as a binary stream encoded
in whatever way the mode indicates.

.. todo:

  Provide a diagram of the format.

Immutability
~~~~~~~~~~~~~

A segment is immutable. That means that we never actually update any oid in
the system. Instead, we create a new oid when updating a value, and append it
to the column. From time to time the system will garbage collect segments in
order to reclaim space.

Rows and MVCC
~~~~~~~~~~~~~~

The system uses `MVCC <http://en.wikipedia.org/wiki/Multiversion_concurrency_control>`_
to manage transactions. There is another structure that lives both in memory and
on disk. This is the row column. It stores a mapping between transactions, row
ids, and logical column addresses, or oids.

If a row has been updated in a transaction, there will be an entry for that row
with the transaction id. This forms a key. The associated value is an array
of oids. If an oid is zero, it means that the column's value is null for that
row. No actual oids of zero are ever allocated.

When a row is updated a new entry is inserted with a new transaction id and
the row id being updated. The value is copied from the last transaction that
updated this row, and the new oids for this row replace the respective values.
This entry is written the row column.

We only keep transactions until the
y are no longer referenced. For example, if
we write every column in a row in transaction 1, and then write every column in
the same row in transaction 2, transaction 1 will be tombstoned. When the
row column is rewritten this transaction will be dropped. If it only ever
lived in memory it will not be written to disk.

Garbage Collection
~~~~~~~~~~~~~~~~~~~

From time to time a segment will be evaluated for garbage collection. The
collection threshold is configurable. All of the oids in the segment are
compared against the oids in the row-column. If an oid is not found, then it
is marked for deletion. If the number of dead oids in the segment is above the
threshold then the segment is rewritten, and the old file is deleted.

Indexes
--------

Indexes are automatic. As other databases of this sort, we believe we can do a
better job by deciding when to create indexes than by having a human try to
guess what the actual query pattern will be.

As for the exact format these will take, a decision has not been made. It would
be nice to avoid duplicating data as much as possible. However, some duplication
is inevitable.

The thinking for now is that each segment may have an associated index. The
index itself may be implemented as:

  * A map between the oid and the index where the oid *should* be for sorted
    behavior. This has the downside of requiring an indirection for searching.
  * An on-disk hash map with pre-sized buckets. The bucket would contain a list
    of the oids that hash to it. (The hash is of course generated by hashing
    the value for that oid.) This is great for point lookups, but not range
    queries.
  * A trie could be used, or somewhat equivalently a prefix hash table.
  * A btree (although this is probably overkill because segments will not
    be that large.)

Ultimately the large scale structure of the indexes are simple merge lists.

.. seealso::

  `MonetDB CREATE INDEX <https://www.monetdb.org/Documentation/Manuals/SQLreference/Indices>`_
    MonetDB may ignore a create index command.

  `The Power of Projections <http://www.vertica.com/2011/09/06/the-power-of-projections-part-3/>`_
    What Vertica does instead of normal indexes.


Consensus
----------

Ideally the hope is that `ZooKeeper <http://zookeeper.apache.org/>`_ can be used
for consensus in the cluster. If not, `Raft <https://raftconsensus.github.io/>`_
will need to be implemented.
