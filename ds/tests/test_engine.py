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
        # Should not be able to drop table
        result = dispatch_command(
            self.db,
            {"cmd": 2,
             "name": "my_test_table"
            }
        )

        self.assertFalse(result["status"], "Table should not exist.")

        self.test_create_table()

        # Should be able to drop table
        result = dispatch_command(
            self.db,
            {"cmd": 2,
             "name": "my_test_table"
            }
        )

        self.assertTrue(result["status"], result.get("error"))

        # Should not be able to drop table
        result = dispatch_command(
            self.db,
            {"cmd": 2,
             "name": "my_test_table"
            }
        )

        self.assertTrue(result["status"], "Table should not exist after drop.")

    def test_put(self):
        self.test_create_table()

        result = dispatch_command(
            self.db,
            {"cmd": 3,
             "name": "my_test_table",
             "columns": ["test_col_1"],
             "values": [[1], [110], [97], [2], [3], [4], [7], [-1]]
            }
        )

        self.assertTrue(result["status"], result.get("error"))

    def test_get(self):
        self.test_put()

        result = dispatch_command(
            self.db,
            {"cmd": 4,
             "name": "my_test_table",
             "predicates": [
                 {"op": "lt", "args": [
                     [0, "test_col_1"],
                     [1, 4]
                 ]}
             ]
            }
        )

        self.assertTrue(result["status"], result.get("error"))
        self.assertEqual(4, len(result["data"]), "Data should have matched 4 rows.")

