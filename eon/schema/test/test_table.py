import random
from eon.schema.data import DataType
from eon.schema.table import Table
from eon.schema.column import Column

__author__ = 'Christopher Nelson'

import unittest


class SchemaColumnTest(unittest.TestCase):
    def setUp(self):
        self.c0 = Column("column_0", DataType.standard_int)
        self.c1 = Column("column_1", DataType.big_int)
        self.c2 = Column("column_2", DataType.small_int)

    def test_create(self):
        t = Table("test", [self.c0, self.c1, self.c2])
        self.assertEqual("test", t.name)

    def test_insert_scalar(self):
        t = Table("test", [self.c0, self.c1, self.c2])
        r = t.insert({"column_0": 1, "column_1": 100, "column_2": 98})
        self.assertTrue(r[0])
        self.assertEqual(0, r[1])

    def test_insert_many_scalars(self):
        t = Table("test", [self.c0, self.c1, self.c2])

        c1 = random.sample(range(1, 10000), 1800)
        c2 = random.sample(range(1, 10000), 1800)
        c3 = random.sample(range(1, 10000), 1800)

        for i in range(0, len(c1)):
            with self.subTest(i=i):
                r = t.insert({"column_0": c1[0], "column_1": c2[0], "column_2": c3[0]})
                self.assertTrue(r[0])
                self.assertEqual(i, r[1])

    def test_insert_vector(self):
        t = Table("test", [self.c0, self.c1, self.c2])

        c1 = random.sample(range(1, 10000), 1800)
        c2 = random.sample(range(1, 10000), 1800)
        c3 = random.sample(range(1, 10000), 1800)

        r = t.insert({"column_0": c1, "column_1": c2, "column_2": c3})
        self.assertTrue(r[0])
        self.assertEqual([i for i in range(0, len(c1))], r[1])
