__author__ = 'Christopher Nelson'

import os
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
        dispatch_command(self.db, {"cmd": 1,
                                   "name": "my_test_table",
                                   "columns": [
                                       {"name": "test_col_1", "type": "int"},
                                       {"name": "test_col_2", "type": "varchar"}
                                   ]
        })

