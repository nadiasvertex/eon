"""
Stores the schema of all active databases.
"""
from eon.store.row import Row

__author__ = 'Christopher Nelson'


class Column:
    def __init__(self, name=None, data_type=None, nullable=True, default=None, max_length=None):
        self.name = name
        self.data_type = data_type
        self.nullable = nullable
        self.default = default
        self.max_length = max_length

    def store(self):
        return {
            "name": self.name,
            "data-type": self.data_type,
            "nullable": self.nullable,
            "default": self.default,
            "max-length": self.max_length
        }

    def load(self, data):
        self.name = data["name"]
        self.data_type = data["data-type"]
        self.nullable = data["nullable"]
        self.default = data["default"]
        self.max_length = data["max-length"]
        return self


class Table:
    def __init__(self, name=None, columns=[]):
        self.name = name
        self.columns = columns
        self.col_map = None

        # Maximum number of rows in a segment.
        self.segment_row_limit = 1 << 16

        self.in_flight = Row(0, self.get_row_type())
        self.archive = []

    def _optimize(self):
        pass

    def get_row_type(self):
        """
        Provides a generator that yields the row type for this table.
        :return: A generator for the row type of this table.
        """
        return (c.data_type for c in self.columns)

    def write(self, data):
        """
        Data must be a dictionary. Each key is a column
        :param data:
        :return:
        """

    def store(self):
        return {
            "name": self.name,
            "columns": [c.store() for c in self.columns],
            "segment_row_limit": self.segment_row_limit
        }

    def load(self, data):
        self.name = data["name"]
        self.columns = [Column().load(cd) for cd in data["columns"]]
        self.segment_row_limit = data["segment_row_limit"]
        return self


class Database:
    def __init__(self, name=None, tables=[]):
        self.name = None
        self.tables = tables

    def store(self):
        return {
            "name": self.name,
            "tables": [t.store() for t in self.tables]
        }

    def load(self, data):
        self.name = data["name"]
        self.tables = [Table().load(t) for t in data["tables"]]




