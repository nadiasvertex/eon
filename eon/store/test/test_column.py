import random
import unittest

from eon.schema.data import DataType
from eon.store.column import Column
from eon.query.plan import Op


__author__ = 'Christopher Nelson'


class ColumnTest(unittest.TestCase):
    def test_create(self):
        _ = Column(DataType.standard_int)

    def test_insert(self):
        c = Column(DataType.standard_int)
        data = random.sample(range(1, 10000), 1800)
        indices = [c.insert(i) for i in data]

        # Make sure that the length of data and the length
        # of indices returned is the same.
        self.assertEqual(len(data), len(indices))

        for i, v in enumerate(data):
            with self.subTest(i=i):
                self.assertEqual(v, c.get(i))

    def test_logical_op(self):
        c = Column(DataType.standard_int)
        data = random.sample(range(1, 1 << 16), 1 << 15)
        for i in data:
            c.insert(i)

        pivot = random.randint(1, 1 << 16)
        r = c.vector_op(Op.lt, pivot)

        for i, v in enumerate(data):
            with self.subTest(i=i):
                self.assertEqual(v < pivot, r[i])


class FrozenColumnTest(unittest.TestCase):
    def test_create(self):
        c = Column(DataType.standard_int)
        _ = c.freeze()

    def test_create_with_data(self):
        c = Column(DataType.standard_int)
        data = random.sample(range(1, 10000), 1800)
        for i in data:
            c.insert(i)

        _ = c.freeze()

    def test_logical_op(self):
        c = Column(DataType.standard_int)
        data = random.sample(range(1, 1 << 16), 1 << 15)
        for i in data:
            c.insert(i)

        # Freeze the column, get the new column and the index map.
        fc, m = c.freeze()

        pivot = random.randint(1, 1 << 16)
        r = fc.vector_op(Op.lt, pivot)
        for i, v in enumerate(r):
            with self.subTest(i=i):
                idx = m[i]
                self.assertEqual(data[idx] < pivot, v)


