import struct

from engine.column.storage.columnar.rtype import dt_to_tc


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
# 2. snappy compression
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
# The rowid array doesn't actually contain any row ids. It has a single entry which indicates the base rowid. The row
# can then be directly indexed by using the difference from the base and the requested rowid. The values in the array
# are value-offsets. Each value-offset is the offset from the beginning of the column array
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
#      numeric it will also be stored as a varint.)
#
#   3. Arity-encoding (Similar to run-length, except that all values in the element are processed for similarity.
#      Storage is (value, count), count is always a varint. The value is varint encoded if it's numeric.)
#
# The value array is designed to be accessed randomly from the row-id array, but must be expanded in memory for
# a binary search. The store can be told to favor space over speed, in which case operations will traverse the
# array directly, decoding on the fly.

header_format = "<QQQQQQQIBB"


class Header:
    def __init__(self, parts):
        self.entry_count = parts[0]
        self.min_row = parts[1]
        self.max_row = parts[2]
        self.min_value_ptr = parts[3]
        self.max_value_ptr = parts[4]
        self.row_array_ptr = parts[5]
        self.val_array_ptr = parts[6]
        self.temperature = parts[7]
        self.data_type = parts[8]
        self.compression_type = parts[9]

    def save(self, data_file):
        data_file.seek(0)
        struct.pack_into(
            header_format, 0,
            self.entry_count,
            self.min_row, self.max_row,
            self.max_value_ptr, self.max_value_ptr,
            self.row_array_ptr, self.val_array_ptr,
            self.temperature,
            self.data_type, self.compression_type)


class Durabase:
    def __init__(self, data_type, data_path):
        self.data_type = data_type
        self.type_code = dt_to_tc.get(data_type)
        self.element_limit = 5000
        self.next_segment_id = 0
        self.data_path = data_path

    def get_next_segment_id(self):
        v = self.next_segment_id
        self.next_segment_id += 1
        return v

    def get_element_header(self, data_file):
        data_file.seek(0)
        parts = struct.unpack_from(header_format, data_file)
        return Header(parts)


