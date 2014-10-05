import unittest
from engine.collection.interval import Interval

__author__ = 'Christopher Nelson'


class TestStandardStore(unittest.TestCase):
    def test_insert_one(self):
        a = Interval.insert([], 1)
        self.assertEqual([(1, 1)], a)

    def test_insert_upper_bound(self):
        a = Interval.insert([], 1)
        Interval.insert(a, 2)
        self.assertEqual([(1, 2)], a)

    def test_insert_three(self):
        a = Interval.insert([], 1)
        Interval.insert(a, 3)
        self.assertEqual([(1, 1), (3, 3)], a)

    def test_insert_lower_bound(self):
        a = Interval.insert([], 5)
        Interval.insert(a, 4)
        self.assertEqual([(4, 5)], a)

    def test_insert_complex_lower_bound(self):
        a = []
        for i in range(0, 10):
            Interval.insert(a, i)

        self.assertEqual([(0, 9)], a)

        for i in range(0, 10):
            Interval.insert(a, i)

        self.assertEqual([(0, 9)], a)

        for j in range(0, 1000, 20):
            for i in range(j, j + 10):
                Interval.insert(a, i)

        self.assertEqual([(j,j+9) for j in range(0, 1000, 20)], a)

    def test_insert_complex_upper_bound(self):
        a = []
        for i in range(9, -1, -1):
            Interval.insert(a, i)

        self.assertEqual([(0, 9)], a)

        for i in range(9, -1, -1):
            Interval.insert(a, i)

        self.assertEqual([(0, 9)], a)

        for j in range(980, -1, -20):
            for i in range(j+9, j-1, -1):
                Interval.insert(a, i)

        self.assertEqual([(j,j+9) for j in range(0, 1000, 20)], a)

