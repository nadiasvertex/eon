import numpy as np

from eon.query.evaluate import Evaluate
from eon.schema.table import Table

__author__ = 'Christopher Nelson'


class Database:
    def __init__(self, name=None, tables=list()):
        self.name = name
        self.tables = tables
        self.table_names = None
        self._init_stage_2()

    def _init_stage_2(self):
        self.table_names = {table.name: table for table in self.tables}

    def modify(self, command):
        """
        Modifies the database schema.
        :param command: The DDL command to execute
        :return:
        """
        pass

    def create_table(self, table):
        """
        Creates a table in the database.

        :param table: The table to make part of this database.
        """
        if self.table_names is None:
            self.table_names = {}

        self.tables.append(table)
        self.table_names[table.name] = table

    def get_table(self, name):
        """
        Provides the table object with the given name.

        :param name: The name of the table.
        :return: A table, or None if there is no table with that name.
        """
        return self.table_names.get(name)

    def query(self, q, plan):
        """
        Queries the database using the given query plan.

        :param q: The query to execute.
        :param plan: The plan to execute the query with.
        :return: The query data.
        """
        e = Evaluate(self, plan)
        table = self.table_names.get(q["from"])
        result_rows = []
        for row in table.rows():
            to_select = e.evaluate(row)
            result_rows.append([c for i, c in enumerate(row) if to_select[i]])

        return result_rows

    def store(self):
        return {
            "name": self.name,
            "tables": [t.store() for t in self.tables]
        }

    def load(self, data):
        self.name = data["name"]
        self.tables = [Table().load(t) for t in data["tables"]]
        self._init_stage_2()
