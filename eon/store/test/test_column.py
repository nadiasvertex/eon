import random
import unittest

import numpy as np

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

    def test_vector_join(self):
        c = Column(DataType.standard_int)
        data = random.sample(range(1, 1 << 16), 10)
        for i in data:
            c.insert(i)

        random_items = [random.choice(data) for _ in range(0, len(data) >> 1)]
        array = np.array(random_items + random_items)
        for item in c.vector_join(array):
            with self.subTest():
                self.assertEqual(2, len(item))
                l, r = item
                self.assertEqual(1, len(set(l)))
                self.assertEqual(len(r), len(set(r)))

                dv = data[l[0]]
                r_indexes = [i for i, v in enumerate(array) if v == dv]
                self.assertListEqual(r_indexes, list(r))


    def test_stream_join(self):
        c = Column(DataType.standard_int)
        data = random.sample(range(1, 1 << 16), 1 << 15)
        for i in data:
            c.insert(i)

        random_items = [random.choice(data) for _ in range(0, len(data) >> 1)]
        array = np.array(random_items + random_items)
        for item in c.stream_join(array):
            with self.subTest():
                self.assertEqual(2, len(item))
                l, r = item
                lv = data[l]
                rv = array[r]
                self.assertEqual(lv, rv)

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

    def test_vector_join(self):
        c = Column(DataType.standard_int)
        data = random.sample(range(1, 1 << 16), 10)
        for i in data:
            c.insert(i)

        fc, m = c.freeze()
        del c

        random_items = [random.choice(data) for _ in range(0, len(data) >> 1)]
        array = np.array(random_items + random_items)
        for item in fc.vector_join(array):
            with self.subTest():
                self.assertEqual(2, len(item))
                l, r = item
                self.assertEqual(1, len(set(l)))
                self.assertEqual(len(r), len(set(r)))

                dv = data[m[l[0]]]
                r_indexes = [i for i, v in enumerate(array) if v == dv]
                self.assertListEqual(r_indexes, list(r))

    def test_stream_join(self):
        c = Column(DataType.standard_int)
        data = random.sample(range(1, 1 << 16), 1 << 15)
        for i in data:
            c.insert(i)

        fc, m = c.freeze()
        del c

        random_items = [random.choice(data) for _ in range(0, len(data) >> 1)]
        array = np.array(random_items + random_items)
        for item in fc.stream_join(array):
            with self.subTest():
                self.assertEqual(2, len(item))
                l, r = item
                lv = data[m[l]]
                rv = array[r]
                self.assertEqual(lv, rv)

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


