"""
Stores the schema of all active databases.
"""

from eon.schema.table import Table


__author__ = 'Christopher Nelson'


class Database:
    def __init__(self, name=None, tables=[]):
        self.name = None
        self.tables = tables

    def modify(self, command):
        """
        Modifies the database schema.
        :param command: The DDL command to execute
        :return:
        """

    def store(self):
        return {
            "name": self.name,
            "tables": [t.store() for t in self.tables]
        }

    def load(self, data):
        self.name = data["name"]
        self.tables = [Table().load(t) for t in data["tables"]]




