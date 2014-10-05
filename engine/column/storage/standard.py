import os
import pickle
import struct
import rocksdb
import bisect
from engine.collection.interval import Interval

__author__ = 'Christopher Nelson'


class StandardTransaction:
    def __init__(self, store, writeable):
        self.store = store
        self.writeable = writeable

        self.txn_warehouse = Warehouse("txn" + str(id(self)), store.column_path) if writeable else None
        self.tombstone = set()


    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is not None:
            self.abort()
        self.commit()

    def commit(self):
        pass

    def abort(self):
        pass

    def put(self, row_id, value):
        self.txn_warehouse.put(row_id, value)

    def get(self, row_id):
        if self.txn_warehouse is not None:
            value = self.txn_warehouse.get(row_id)
            if value is not None:
                return value

        for txn in reversed(self.store.committed):
            value = txn.get(row_id)
            if value is not None:
                return value

        return self.store.get(row_id)

    def unique(self):
        if self.store.warehouse.index is None:
            self.store.warehouse.create_index()

        for value in self.store.warehouse.values():
            yield value


class Warehouse:
    def __init__(self, name, column_path):
        self.column_path = column_path
        self.name = name
        db_options = rocksdb.Options(create_if_missing=True)
        self.warehouse = rocksdb.DB(os.path.join(self.column_path, name),
                                    db_options)

        self.index = None

    def _update_index(self, row_id, value):
        row_intervals_packed = self.index.get(value)
        row_intervals = \
            [] if row_intervals_packed is None else \
                pickle.loads(row_intervals_packed)
        Interval.insert(row_intervals, row_id)
        self.index.put(value, pickle.dumps(row_intervals))


    def put(self, row_id, value):
        self.warehouse.put(struct.pack("=Q", row_id), value)
        if self.index is None:
            return
        self._update_index(row_id, value)

    def get(self, row_id):
        row_key = struct.pack("=Q", row_id)
        return self.warehouse.get(row_key)

    def values(self):
        it = self.warehouse.itervalues()
        it.seek_to_first()
        for value in list(it):
            yield value

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

        it = self.warehouse.iteritems()
        it.seek_to_first()

        # Go through the warehouse and insert each value as a key in the index. Map the keys to the rows they
        # come from. If a key is found more than once it will map to an interval list, so the key will appear only
        # once in the data.
        for row_key, value in list(it):
            row_id = struct.unpack_from("=Q", row_key)
            self._update_index(row_id, value)


class StandardStore:
    def __init__(self, name, table_path):
        self.column_path = os.path.join(table_path, name)
        if not os.path.exists(self.column_path):
            os.makedirs(self.column_path)

        self.warehouse = Warehouse("warehouse", self.column_path)

        # Holds committed transactions. These are moved into the warehouse in the
        # background, but until they are fully transferred they live here. The
        # earliest transactions are at the front, the latest at the back.
        self.committed = []


    def begin(self, write=False):
        return StandardTransaction(self, write)
