import subprocess
import unittest

from eon.query.parser import Parser
from eon.schema.column import Column
from eon.schema.data import DataType
from eon.schema.db import Database
from eon.schema.table import Table

__author__ = 'Christopher Nelson'


class TestParser(unittest.TestCase):
    def _get_output(self, p, test_data):
        process = subprocess.Popen([p.runnable_path], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        out, _ = process.communicate(input="\n".join(test_data).encode("utf-8"))

        return [l.strip() for l in out.decode("utf-8").split("\n") if l]

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

        test_data = [
            "5",
            "1 2 3 10 20",
            "5",
            "20 10 3 2 1",
            "10",
            "1 2 3 10 20 20 10 3 2 1",
            "0"
        ]

        self.assertEqual(
            ["5", "T T T F F", "5", "F F T T T", '10', 'T T T F F F F T T T'],
            self._get_output(p, test_data)
        )

    def test_gt(self):
        p = Parser(self.db, {
            "from": "test",
            "where": {
                "all": [
                    {"op": ">", "left": "column_0", "right": 5},
                ]
            }
        })

        self.assertTrue(p.compile(), msg=p.get_message())

        program = p.get_program()
        print(program)
        self.assertIsNotNone(program)
        self.assertLess(0, len(program))

        test_data = [
            "5",
            "1 2 3 10 20",
            "5",
            "20 10 3 2 1",
            "10",
            "1 2 3 10 20 20 10 3 2 1",
            "0"
        ]

        self.assertEqual(
            ["5", "F F F T T", "5", "T T F F F", '10', 'F F F T T T T F F F'],
            self._get_output(p, test_data)
        )

    def test_eq(self):
        p = Parser(self.db, {
            "from": "test",
            "where": {
                "all": [
                    {"op": "=", "left": "column_0", "right": 5},
                ]
            }
        })

        self.assertTrue(p.compile(), msg=p.get_message())

        program = p.get_program()
        print(program)
        self.assertIsNotNone(program)
        self.assertLess(0, len(program))

        test_data = [
            "5",
            "1 2 5 10 20",
            "5",
            "20 10 5 2 1",
            "10",
            "1 2 5 10 20 20 10 5 2 1",
            "0"
        ]

        self.assertEqual(
            ["5", "F F T F F", "5", "F F T F F", '10', 'F F T F F F F T F F'],
            self._get_output(p, test_data)
        )

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

        test_data = [
            "5",
            "1 2 3 10 20",
            "100 5 6 7 8",
            "5",
            "20 10 3 2 1",
            "8 7 6 5 100",
            "10",
            "1 2 3 10 20 20 10 3 2 1",
            "8 7 6 5 100 100 5 6 7 8",
            "0"
        ]

        self.assertEqual(
            ["5", "F T T F F", "5", "F F T T F", '10', 'T T T F F F F T T T'],
            self._get_output(p, test_data)
        )
