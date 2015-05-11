import random
import unittest

import numpy as np

from eon.schema.data import DataType
from eon.store.row import Row


__author__ = 'Christopher Nelson'


class RowTest(unittest.TestCase):
    def test_create(self):
        row_type = (DataType.big_int, DataType.standard_int, DataType.small_int)
        _ = Row(0, row_type)

    def test_insert(self):
        row_type = (DataType.big_int, DataType.standard_int, DataType.small_int)
        r = Row(0, row_type)

        c1 = random.sample(range(1, 10000), 1800)
        c2 = random.sample(range(1, 10000), 1800)
        c3 = random.sample(range(1, 10000), 1800)

        for i in range(0, len(c1)):
            r.insert((True, True, True), (c1[i], c2[i], c3[i]))

        self.assertEqual(len(c1), len(r.data))

        for i in range(0, len(c1)):
            d = r.get(i)
            with self.subTest(i=i):
                self.assertEqual(c1[i], d[0])
                self.assertEqual(c2[i], d[1])
                self.assertEqual(c3[i], d[2])

    def test_insert_null(self):
        row_type = (DataType.big_int, DataType.standard_int, DataType.small_int)
        r = Row(0, row_type)

        c1 = random.sample(range(1, 10000), 1800)
        c2 = random.sample(range(1, 10000), 1800)
        c3 = random.sample(range(1, 10000), 1800)

        b1 = [random.choice((True, False)) for _ in range(0, len(c1))]
        b2 = [random.choice((True, False)) for _ in range(0, len(c2))]
        b3 = [random.choice((True, False)) for _ in range(0, len(c2))]

        for i in range(0, len(c1)):
            r.insert((b1[i], b2[i], b3[i]), (c1[i], c2[i], c3[i]))

        self.assertEqual(len(c1), len(r.data))

        for i in range(0, len(c1)):
            d = r.get(i)
            with self.subTest(i=i):
                self.assertEqual(c1[i] if b1[i] else None, d[0])
                self.assertEqual(c2[i] if b2[i] else None, d[1])
                self.assertEqual(c3[i] if b3[i] else None, d[2])

    def test_select(self):
        row_type = (DataType.big_int, DataType.standard_int, DataType.small_int)
        r = Row(0, row_type)

        c1 = random.sample(range(1, 10000), 1800)
        c2 = random.sample(range(1, 10000), 1800)
        c3 = random.sample(range(1, 10000), 1800)

        for i in range(0, len(c1)):
            r.insert((True, True, True), (c1[i], c2[i], c3[i]))

        self.assertEqual(len(c1), len(r.data))

        for i in range(0, len(c1)):
            d = r.select((True, False, False), i)
            self.assertEqual(1, len(d))
            with self.subTest(i=i):
                self.assertEqual(c1[i], d[0])

            d = r.select((False, True, False), i)
            self.assertEqual(1, len(d))
            with self.subTest(i=i):
                self.assertEqual(c2[i], d[0])

            d = r.select((False, False, True), i)
            self.assertEqual(1, len(d))
            with self.subTest(i=i):
                self.assertEqual(c3[i], d[0])

    def test_join(self):
        base_rid = (1 << 16) * 3
        row_type = (DataType.big_int, DataType.standard_int, DataType.small_int)
        row = Row(base_rid, row_type)

        c1 = random.sample(range(1, 1 << 16), 1 << 15)
        c2 = random.sample(range(1, 1 << 16), 1 << 15)
        c3 = random.sample(range(1, 1 << 16), 1 << 15)

        for i in range(0, len(c1)):
            row.insert((True, True, True), (c1[i], c2[i], c3[i]))

        random_items = [random.choice(c1) for _ in range(0, len(c1) >> 1)]
        array = np.array(random_items + random_items)
        for item in row.join(0, array):
            with self.subTest():
                self.assertEqual(2, len(item))
                l, r = item
                for i, v in enumerate(l):
                    with self.subTest(i=i):
                        self.assertLessEqual(base_rid, v)
