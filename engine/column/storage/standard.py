import os
import struct
import rocksdb

__author__ = 'Christopher Nelson'


class StandardTransaction:
    def __init__(self, store, writeable):
        self.store = store
        self.writeable = writeable
        self.snapshot = store.warehouse.snapshot()

        self.tws = rocksdb.DB(os.path.join(store.column_path, "txn" + str(id(self))),
                              store.db_options)

        self.tombstone = set()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is not None:
            self.abort()
        self.commit()

    def put(self, row_id, value):
        self.tws.put(struct.pack("=Q", row_id), value)

    def get(self, row_id):
        row_key = struct.pack("=Q", row_id)
        value = self.tws.get(row_key)
        if value is not None:
            return value

        for txn in reversed(self.store.committed):
            value = txn.get(row_key)
            if value is not None:
                return value

        return self.store.get(row_key, snapshot=self.snapshot)





class StandardStore:
    def __init__(self, name, table_path):
        self.column_path = os.path.join(table_path, name)
        if not os.path.exists(self.column_path):
            os.makedirs(self.column_path)

        self.db_options = rocksdb.Options(create_if_missing=True)
        self.warehouse = rocksdb.DB(os.path.join(self.column_path, "warehouse"),
                                    self.db_options)

        # Holds committed transactions. These are moved into the warehouse in the
        # background, but until they are fully transferred they live here. The
        # earliest transactions are at the front, the latest at the back.
        self.committed = []

    def begin(self, write=False):
        return StandardTransaction(self, write)
