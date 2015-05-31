__author__ = 'Christopher Nelson'

class Database:
    def __init__(self, c, name, schema=None):
        self.c = c
        self.name = name
        self.schema = schema

    def create_table(self, table_name, columns):
        """
        Creates a new table in the database.
        :param table_name: The table name.
        :param columns: A list of column definitions.
        """
        table_ddl = {
            "columns": columns
        }
        result = yield from self.c.cmd("/".join(["s", self.name, table_name]), payload=table_ddl, method="PUT")
        self.c.check_for_ddl_error(result)

