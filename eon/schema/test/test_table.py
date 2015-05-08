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
        t = Table([self.c0, self.c1, self.c2])

    def test_insert_scalar(self):
        t = Table([self.c0, self.c1, self.c2])

