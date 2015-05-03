import random
import unittest

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
