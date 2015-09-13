import unittest

from eon.query.parser import Parser
from eon.schema.column import Column
from eon.schema.data import DataType
from eon.schema.db import Database
from eon.schema.table import Table

__author__ = 'Christopher Nelson'


class TestParser(unittest.TestCase):
    def setUp(self):
        self.c0 = Column("column_0", DataType.standard_int)
        self.c1 = Column("column_1", DataType.big_int)
        self.c2 = Column("column_2", DataType.small_int)
        self.t = Table("test", [self.c0, self.c1, self.c2])
        self.db = Database("test_db", tables=[self.t])

    def test_lt(self):
        p = Parser(self.db, {
            "from": "test",
            "where": {
                "all": [
                    {"op": "<", "left": "column_0", "right": 5},
                ]
            }
        })

        self.assertTrue(p.compile(), msg=p.get_message())

        program = p.get_program()
        print(program)
        self.assertIsNotNone(program)
        self.assertLess(0, len(program))

    def test_two_column_lt(self):
        p = Parser(self.db, {
            "from": "test",
            "where": {
                "all": [
                    {"op": "<", "left": "column_0", "right": 5},
                    {"op": "<", "left": "column_1", "right": 19},
                ]
            }
        })

        self.assertTrue(p.compile(), msg=p.get_message())

        program = p.get_program()
        print(program)
        self.assertIsNotNone(program)
        self.assertLess(0, len(program))
