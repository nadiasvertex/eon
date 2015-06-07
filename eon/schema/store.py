"""
Stores the schema of all active databases.
"""
from datetime import datetime
import getpass
import hashlib
import json
import logging
import os
import random
import string

from eon.error import code
from eon.schema.column import Column
from eon.schema.data import DataType

from eon.schema.db import Database
from eon.schema.table import Table

__author__ = 'Christopher Nelson'

NEW_PASSWORD_LENGTH = 10

data_type_values = [i.value for i in DataType]
required_column_ddl_fields = ("name", "data-type")
default_column_ddl_values = {"nullable": True, "default": None, "max-length": None}
scalar_value_types = (DataType.big_int, DataType.standard_int, DataType.small_int, DataType.bool)


class Store:
    def __init__(self, data_dir, default_values={}):
        self.log = logging.getLogger()
        self.data_dir = data_dir
        self.configuration = None
        self.databases = {}

        if not os.path.exists(data_dir):
            self.log.info("Creating data directory '%s'", data_dir)
            os.makedirs(data_dir)

        self.store_file = os.path.join(self.data_dir, "store.json")
        if not os.path.exists(self.store_file):
            new_password = default_values.get("password", self._pass_gen())
            initial_data = {
                "created": datetime.now().isoformat(),
                "modified": datetime.now().isoformat(),
                "admin": getpass.getuser(),
                "pass": hashlib.sha512(new_password.encode("utf-8")).hexdigest(),
                "databases": []
            }
            with open(self.store_file, "wb") as o:
                o.write(json.dumps(initial_data).encode("utf-8"))

            self.log.info("New data site initialized at '%s'", self.data_dir)

            if "password" not in default_values:
                self.log.info(
                    "Password for new instance is '%s'. You should change this as "
                    "soon as possible for production use.", new_password
                )

        self.load_site()

    def _pass_gen(self):
        """
        Generates a secure random password.
        :return: The new password.
        """

        r = random.SystemRandom()
        return ''.join(
            [r.choice(string.ascii_letters + string.digits + string.punctuation)
             for _ in range(NEW_PASSWORD_LENGTH)]
        )

    def load_site(self):
        with open(self.store_file, "rb") as i:
            self.configuration = json.loads(i.read().decode("utf-8"))

        self.log.info("Loaded configuration from '%s'", self.store_file)
        for db in self.configuration["databases"]:
            dbo = Database()
            dbo.load(db)

            self.databases[dbo.name] = dbo
            self.log.debug("loaded '%s'", dbo.name)

    def store_site(self):
        database_config = []
        for db in self.databases.values():
            database_config.append(db.store())

        self.configuration["databases"] = database_config
        with open(self.store_file, "wb") as o:
            o.write(json.dumps(self.configuration).encode("utf-8"))

        self.log.info("Stored configuration in '%s'", self.store_file)

    def get_database(self, name):
        return self.databases.get(name)

    def create_database(self, name):
        db = Database(name=name)
        self.databases[name] = db

        return db

    def create_table(self, db_name, table_name, table_ddl):
        """
        Create a new table.

        :param db_name: The database name.
        :param table_name: The table name to create.
        :param table_ddl: The table data structure language.
        :return: False, error_code, formatting_arguments on failure; or True, None, None on success.
        """
        db = self.databases.get(db_name)
        if db is None:
            return False, code.UNKNOWN_SCHEMA_OBJECT, {"name": db_name}

        table = db.get_table(table_name)
        if table is not None:
            return False, code.DUPLICATE_SCHEMA_OBJECT, {"name": ".".join([db_name, table_name])}

        columns = table_ddl.get("columns")
        if columns is None:
            return False, code.MALFORMED_SCHEMA_SPEC, {"required_field_name": "columns"}

        new_columns = []
        for i, column in enumerate(columns):
            # Check for required fields
            for field in required_column_ddl_fields:
                if field not in column:
                    return False, code.MALFORMED_SCHEMA_SPEC, {
                        "required_field_name": "columns[%d].%s" % (i, field)
                    }

            # Make sure the field values are within required limits.
            if column["data-type"] not in data_type_values:
                return False, code.UNKNOWN_DATA_TYPE, {
                    "value": column["data-type"],
                    "allowed_values": {i.name: i.value for i in DataType}
                }

            if "max-length" in column:
                dt = DataType(column["data-type"])
                max_length = column["max-length"]
                if max_length is not None:
                    if max_length <= 0 or dt in scalar_value_types:
                        return False, code.ILLEGAL_DATA_LENGTH, {
                            "name": dt,
                            "value": max_length
                        }

            # Fill in default values
            new_column_v = default_column_ddl_values.copy()
            new_column_v.update(column)
            new_column = Column()
            new_column.load(new_column_v)
            new_columns.append(new_column)

        # Create the new table
        table = Table(name=table_name, columns=new_columns)
        db.create_table(table)

        return True, None, None
