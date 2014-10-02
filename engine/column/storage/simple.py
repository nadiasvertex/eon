import os
import leveldb

__author__ = 'Christopher Nelson'


class StandardTransaction:
    def __init__(self, warehouse, versions, target):
        """
        The transactional environment creates a new database for each transaction. Writes are only performed on the
        target database.

        :param warehouse:
        :param versions:
        :param target:
        :return:
        """
        self.warehouse = warehouse
        self.versions = versions
        self.target = target


class StandardStore:
    def __init__(self, name, table_path):
        self.column_path = os.path.join(table_path, name)
        if not os.path.exists(self.column_path):
            os.makedirs(self.column_path)

        self.warehouse = leveldb.LevelDB(os.path.join(self.column_path, "data"), create_if_missing=True)
        self.versions = []

