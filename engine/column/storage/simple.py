import os
import leveldb
import struct

__author__ = 'Christopher Nelson'


class SimpleTransaction:
    def __init__(self, store, writeable):
        """
        The transactional environment creates a new database for each transaction. Writes are only performed on the
        target database.

        :param store:
        :return:
        """
        self.store = store
        self.tws_path = os.path.join(self.column_path, "txn" + str(id(self)))
        self.tws = leveldb.LevelDB(self.tws_path, create_if_missing=True)

    def put(self, row_id, value):
        self.tws.put(struct.pack("=Q", row_id), value)

    def get(self, row_id):
        value = self.tws.get(row_id)
        if value is not None:
            return value

        for txn in reversed(self.store.versions):
            value = txn.get(row_id)
            if value is not None:
                return value






class SimpleStore:
    def __init__(self, name, table_path):
        self.column_path = os.path.join(table_path, name)
        if not os.path.exists(self.column_path):
            os.makedirs(self.column_path)

        self.warehouse = leveldb.LevelDB(os.path.join(self.column_path, "warehouse"), create_if_missing=True)
        self.versions = []

    def begin(self, write=False):
        return SimpleTransaction(self, write)

