__author__ = 'Christopher Nelson'


class Database:
    def __init__(self, c, name, schema=None):
        self.c = c
        self.name = name
        self.schema = schema

    def refresh_schema_cache(self):
        """
        Forces a refresh of the schema cache. If you have no reason to believe that the schema has changed, then
        there's no reason to round trip the server.
        """
        r = self.c.cmd("s/" + self.name, method="GET")
        self.c.check_for_ddl_error(r)
        self.schema = r["schema"]

        # TODO: Use server mechanism to determine if whole schema cache needs updating.

    def create_table(self, table_name, columns):
        """
        Creates a new table in the database.
        :param table_name: The table name.
        :param columns: A list of column definitions.
        """
        table_ddl = {
            "columns": columns
        }
        result = self.c.cmd("/".join(["s", self.name, table_name]), payload=table_ddl, method="PUT")
        self.c.check_for_ddl_error(result)

    def has_table(self, table_name, use_cache=True):
        """
        Checks to see if the table name exists. This call will use the cache,
        unless use_cache is set to False. If use_cache is False, a call will be made
        to the server to refresh the cache.

        :param table_name: The name of the table to check for.
        :param use_cache: If True, use the cache to satisfy this request. Otherwise update the cache.
        :return: True if the table exists, False otherwise.
        """

        if not use_cache:
            self.refresh_schema_cache()

        return table_name in {t["name"] for t in self.schema["tables"]}

    def insert(self, table_name, values):
        """
        Insert data into the table.

        :param table_name: The table to insert data into.
        :param values: This is a dictionary where each column name is a key, and the value is the
                        data for that column. If the value is a list, then the values specify consecutive rows. Every
                        value must have the same amount of data, otherwise data corruption will result.
        :return: A tuple of (True, rid) if it worked, or a list of (True, rid) for each row in the bulk stream that was
                 inserted.
        """
        result = self.c.cmd("/".join(["r", self.name, table_name]), payload=values, method="PUT")
        self.c.check_for_ddl_error(result)
        return result["data"]
