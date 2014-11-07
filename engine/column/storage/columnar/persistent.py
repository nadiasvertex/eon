__author__ = 'Christopher Nelson'

# A store contains the data for a column. It is a sorted, possibly compressed set of data files. Each file contains
# a sorted subset of data. From time to time, individual files in a store may be merged. The general structure is
# very much like a structured merge tree, except that there is not an arbitrary key.
#
# Each data file is broken up into variably-sized blocks. Each block has a small header which indicates the number
# of entries in the block, the minimum and maximum row ids present in the block, the minimum and maximum
# values in the block (or pointers to them for variably sized data), a pointer to the rowid array, and a pointer to
# the value array. There is also a pointer to the next block in the file, a block temperature, and a byte which
# indicates which of the following block-level compression formats have been applied to the block's data:
#
# 0. No compression
# 1. lz4 compression
#   2. snappy compression
#   3. lzo compression
#   4. zlib compression
#   5. bzip compression
#   6. lzma compression
#
#   Note that the store can be told to optimize blocks by reading them in and trying each compression algorithm
#   in turn to find the one which provides the most compression. On the other hand, the optimizer can also be told
#   to find the format which decompresses most quickly for that block. By default lz4 is chosen. The optimize
#   function can be run incrementally during idle periods by the user.
#
# Following that is the rowid array.
#
# The rowid array is a sorted set of tuples (rowid, value-offset). The rowid is delta-encoded from the minimum rowid
# in the header, and stored as a varint. The value-offset is the offset from the beginning of the column array
# encoded as a delta and stored as a varint.
#
# The value array is where things get complex. The values are always stored in some highly compressed format. There
# are several to choose from, and different methods may be used for different blocks in the same file. The first
# byte of the value array is a number which indicates which of the compression formats have been used.
#
#   1. Delta-encoding (There is an assumed starting value of zero, successive values encode only their difference
#      from the last value. The difference is written as a varint.)
#
#   2. Run-length encoding (A set of tuples of (value, length), length is always stored as a varint. If value is
#   numeric it will also be stored as a varint.)
#
# The value array is designed to be accessed randomly from the row-id array, but must be expanded in memory for
# a binary search. The store can be told to favor space over speed, in which case operations will traverse the
# array directly, decoding on the fly.
