__author__ = 'Christopher Nelson'

import shutil
import unittest

import lmdb

from ds.engine import dispatch_command


class TestEngine(unittest.TestCase):
    def setUp(self):
        self.db = lmdb.open("unit_test_db", max_dbs=32768)

    def tearDown(self):
        self.db.close()
        shutil.rmtree("unit_test_db")

    def test_create_table(self):
        result = dispatch_command(
            self.db,
            {"cmd": 1,
             "name": "my_test_table",
             "columns": [
                 {"name": "test_col_1", "type": "int"},
                 {"name": "test_col_2", "type": "varchar"}
             ]
            }
        )

        self.assertTrue(result["status"], result.get("error"))

    def test_drop_table(self):
        result = dispatch_command(
            self.db,
            {"cmd": 2,
             "name": "my_test_table"
            }
        )

        self.assertFalse(result["status"], "Table should not exist.")

        self.test_create_table()

        result = dispatch_command(
            self.db,
            {"cmd": 2,
             "name": "my_test_table"
            }
        )

        self.assertTrue(result["status"], result.get("error"))

    def test_put(self):
        self.test_create_table()

        result = dispatch_command(
            self.db,
            {"cmd": 3,
             "name": "my_test_table",
             "columns": ["test_col_1"],
             "values": [[1],[2],[3],[4],[7],[-1]]
            }
        )

        self.assertTrue(result["status"], result.get("error"))


