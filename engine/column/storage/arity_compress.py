import os
import struct

import lmdb


__author__ = 'Christopher Nelson'

LAST_INDEX_KEY = b'next_index'


class Pilot:
    def __init__(self, threshold=256):
        self.threshold = threshold
        self.values = set()

    def analyze(self, value):
        self.values.add(value)

    def evaluate(self):
        return len(self.values) <= self.threshold


class ArityTransaction:
    def __init__(self, indexed_values_txn, mapped_indexes_txn, row_map_txn):
        self.indexed_values_txn = indexed_values_txn
        self.mapped_indexes_txn = mapped_indexes_txn
        self.row_map_txn = row_map_txn

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is not None:
            self.abort()
        self.commit()

    def commit(self):
        self.indexed_values_txn.commit()
        self.mapped_indexes_txn.commit()
        self.row_map_txn.commit()

    def abort(self):
        self.indexed_values_txn.abort()
        self.mapped_indexes_txn.abort()
        self.row_map_txn.abort()

    def get(self, row_id):
        row_key = struct.pack("=Q", row_id)
        value_index = self.row_map_txn.get(row_key)
        if value_index is None:
            return None
        return self.indexed_values_txn.get(bytes(value_index))

    def put(self, row_id, value):
        value_index = self.mapped_indexes_txn.get(value)

        # If the value isn't in our mapped indexes database, update the map and the index.
        if value_index is None:
            last_index_raw = self.indexed_values_txn.get(LAST_INDEX_KEY)
            last_index = struct.unpack_from("=B", last_index_raw)[0] if last_index_raw is not None else -1
            value_index = struct.pack("=B", last_index + 1)

            self.indexed_values_txn.put(LAST_INDEX_KEY, value_index)
            self.indexed_values_txn.put(value_index, value)
            self.mapped_indexes_txn.put(value, value_index)

        # Update the row with the saved value.
        row_key = struct.pack("=Q", row_id)
        self.row_map_txn.put(row_key, bytes(value_index))

    def unique(self):
        with self.mapped_indexes_txn.cursor() as cursor:
            cursor.first()
            for key in cursor.iternext(keys=True, values=False):
                yield bytes(key)

    def count(self):
        with self.row_map_txn.cursor() as cursor:
            cursor.first()
            entry_count = 0
            while cursor.next():
                entry_count += 1

        return entry_count

    def filter(self, predicate):
        with self.mapped_indexes_txn.cursor() as cursor:
            cursor.first()
            keepers = {bytes(v) for k, v in cursor if predicate(k)}

        with self.row_map_txn.cursor() as cursor:
            cursor.first()
            for k, v in cursor:
                vb = bytes(v)
                if vb in keepers:
                    yield struct.unpack_from("=B", k)


class ArityCompressedStore:
    def __init__(self, name, table_path):
        self.column_path = os.path.join(table_path, name)
        if not os.path.exists(self.column_path):
            os.makedirs(self.column_path)

        self.row_map_path = os.path.join(self.column_path, "row_map")
        self.indexed_values_path = os.path.join(self.column_path, "value_index")
        self.mapped_indexes_path = os.path.join(self.column_path, "index_map")

        # The following databases keep track of the unique values in the database.
        #

        # This database maps the values by index.
        self.indexed_values = lmdb.open(self.indexed_values_path)

        # This database maps the values to their index.
        self.mapped_indexes = lmdb.open(self.mapped_indexes_path)

        # This database maps the column values to a row. For a given row we only store a byte
        # indicating which index into the unique_values dictionary it is.
        self.row_map = lmdb.open(self.row_map_path, map_size=1 << 40)

    def begin(self, write=False):
        return ArityTransaction(self.indexed_values.begin(write=write, buffers=True),
                                self.mapped_indexes.begin(write=write, buffers=True),
                                self.row_map.begin(write=write, buffers=True))
