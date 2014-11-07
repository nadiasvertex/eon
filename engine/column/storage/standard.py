from copy import copy
import os
import struct

import rocksdb

from engine.column.storage import varint
from engine.schema.isolation import Level


__author__ = 'Christopher Nelson'


class StandardTransaction:
    def __init__(self, store, version, writeable, isolation_level=Level.snapshot):
        self.store = store
        self.writeable = writeable
        self.version = version
        self.warehouse = store.warehouse
        self.db = store.warehouse.db
        self.index = store.warehouse.index

        if isolation_level == Level.read_dirty:
            self.uncommitted = set()
        elif isolation_level == Level.read_committed:
            self.uncommitted = store.warehouse.uncommitted
        else:
            self.uncommitted = copy(store.warehouse.uncommitted)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is not None:
            self.abort()
        self.commit()

    def _version_visible(self, row_version):
        """
        Determines if a particular row is visible by comparing it's write version with the current transaction's
        version. It also considers other concurrent transactions which are not committed.

        :param row_version: The last version this column of this row was written in.
        :return: True if the row version is visible, False otherwise.
        """
        txn_version = self.version
        return row_version == txn_version or (row_version < txn_version and row_version not in self.uncommitted)


    def commit(self):
        if self.writeable:
            self.warehouse.uncommitted.remove(self.version)

    def abort(self):
        if self.writeable:
            self.warehouse.garbage.add(self.version)

    def unique(self):
        """
            Provides a generator that iterates over the unique values in the database.
            """
        if self.index is None:
            self.warehouse.create_index()
            self.index = self.warehouse.index

        wg = self.values()

        while True:
            v = next(wg)
            if v is None:
                return
            yield v

    def put(self, row_id, value):
        """
        Stores a column value in the given row.

        :param row_id: The row id to associate with this column value.
        :param value: The value to store.
        """
        self.db.put(struct.pack("=QQ", row_id, self.version), value)
        if self.index is None:
            return
        self.store.update_index(self.version, row_id, value)

    def get(self, row_id):
        """
        Gets a column value value using the row id.

        :param row_id: The row id to fetch.
        :return: The value of the column, or None if no such row exists.
        """
        row_key = struct.pack("=QQ", row_id, 0)
        it = self.db.iteritems()

        # Position the iterator near the item we want. We may be right there ut probably we will need to
        # iterate through the list to find the right version.
        it.seek(row_key)
        last_value = None
        for k, v in list(it):
            if k == row_key:
                return v

            item_row_id, item_version = struct.unpack_from("=QQ", k)
            if item_row_id != row_id:
                return None

            if item_version > self.version:
                return last_value

            if self._version_visible(item_version):
                last_value = v
        else:
            return last_value

    def values(self):
        """
        Provides a generator that iterates over the unique values in the database.
        """
        if self.index is None:
            self.warehouse.create_index()
            self.index = self.warehouse.index

        it = self.index.iterkeys()
        it.seek_to_first()
        for value in list(it):
            yield value

    def filter(self, predicate):
        """
        Iterate over the unique values in the column and return the row_id of every row with the matching value.
        Requires that :func:create_index has already been called.

        :param txn_version: The version of the row to use.
        :param predicate: The function which indicates if a value matches.
        :return: A generator which will yield every matching row_id.
        """
        if self.index is None:
            self.warehouse.create_index()
            self.index = self.warehouse.index

        it = self.index.iterkeys()
        it.seek_to_first()
        for value in list(it):
            if not predicate(value):
                continue

            packed_row_id_array = self.index.get(value)

            # Decode row/version items in packed row id array
            position = 0
            rows = []
            while position < len(packed_row_id_array):
                row_id, position = varint.decode(packed_row_id_array, position)
                row_version, position = varint.decode(packed_row_id_array, position)
                rows.append((row_id, row_version))

            # TODO: persist the sorted array so we don't have to do this every time.

            # Sort row id/version data so we can count rows correctly in view of multiple versions. This makes
            # sure that we don't accidentally yield each row more than once.
            rows.sort()

            # Find the "best row" for each set of versioned rows, and yield that.
            best_row = None
            for row_id, row_version in rows:
                if row_id != best_row:
                    if best_row != None:
                        yield best_row

                best_row = row_id if self._version_visible(row_version) else None
            else:
                if best_row != None:
                    yield best_row


    def count(self):
        """
        Counts the number of rows in the database. This counts each row once, even if there are multiple versions
        of the row in the database.

        :returns: The number of rows.
        """
        it = self.db.iterkeys()
        it.seek_to_first()
        row_count = 0
        current_row_id = None
        for row_key in list(it):
            row_id, row_version = struct.unpack_from("=QQ", row_key)
            if row_id != current_row_id and self._version_visible(row_version):
                row_count += 1
                current_row_id = row_id

        return row_count


class Garbage:
    def __init__(self, column_path):
        self.column_path = column_path
        self.db = None
        self.garbage = {}

    def _switch_to_disk(self):
        db_options = rocksdb.Options(create_if_missing=True)
        self.db = rocksdb.DB(os.path.join(self.column_path, "garbage"),
                             db_options)

    def delete(self, version, row_id):
        rows = self.garbage.setdefault(version, [])
        rows.append(row_id)


class Warehouse:
    def __init__(self, name, column_path):
        self.column_path = column_path
        self.name = name
        db_options = rocksdb.Options(create_if_missing=True)
        self.db = rocksdb.DB(os.path.join(self.column_path, name),
                             db_options)

        self.index = None
        self.uncommitted = set()
        self.garbage = set()

    def update_index(self, version, row_id, value):
        """
        Update the index by finding the value in the database, and then appending this row id and version onto a list
        of versioned rows. This indicates rows where this value shows up. The row and version data is compressed using
        the varint encoding scheme.

        :param version: The version of the row where this value appears.
        :param row_id: A row where this value shows up.
        :param value: The version of the row where this value shows up.
        """
        row_packed_data = self.index.get(value)
        row_array = \
            bytearray() if row_packed_data is None else \
                bytearray(row_packed_data)
        varint.encode(row_array.append, row_id)
        varint.encode(row_array.append, version)
        self.index.put(value, bytes(row_array))

    def create_index(self):
        """
        Create an index for the column. This is automatically called when certain operations are invoked on a
        column. For example, the unique() function requires the creation of an index to perform its function.
        Once the index is created it will be kept up to date by later writes.
        """
        if self.index is not None:
            return

        db_options = rocksdb.Options(create_if_missing=True)
        self.index = rocksdb.DB(os.path.join(self.column_path, self.name + "-index"),
                                db_options)

        it = self.db.iteritems()
        it.seek_to_first()

        # Go through the warehouse and insert each value as a key in the index. Map the keys to the rows they
        # come from. If a key is found more than once it will map to a row list, so the key will appear only
        # once in the data.
        for row_key, value in list(it):
            row_id, version = struct.unpack_from("=QQ", row_key)
            self.update_index(version, row_id, value)


class StandardStore:
    def __init__(self, name, table_path):
        self.column_path = os.path.join(table_path, name)
        if not os.path.exists(self.column_path):
            os.makedirs(self.column_path)

        self.current_version = 0
        self.warehouse = Warehouse("warehouse", self.column_path)

    def begin(self, write=False, isolation_level=Level.snapshot):
        self.current_version += 1
        version = self.current_version
        if write:
            self.warehouse.uncommitted.add(version)
        return StandardTransaction(self, version, write, isolation_level)
