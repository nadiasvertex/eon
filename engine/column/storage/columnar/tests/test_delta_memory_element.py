import tempfile
import unittest

from engine.column.storage.columnar import memory
from engine.column.storage.columnar.elements import delta_memory
from engine.column.storage.columnar.memory import ResultType


__author__ = 'Christopher Nelson'


class TestElement(unittest.TestCase):
    def setUp(self):
        self.table_path = tempfile.mkdtemp()
        self.mb = memory.Membase("i")

    def test_put(self):
        e = delta_memory.Element(self.mb)
        e.put(7, 101)
        self.assertEqual(1, e.count())

    def test_put_many(self):
        e = delta_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))
        self.assertEqual(9000, e.count())

    def test_get(self):
        e = delta_memory.Element(self.mb)
        e.put(7, 101)

        self.assertEqual(101, e.get(7))

    def test_get_some(self):
        e = delta_memory.Element(self.mb)
        for i in range(1010, 1000, -1):
            e.put(i, int(i*3))

        for i in range(1010, 1000, -1):
            self.assertEqual(e.get(i), int(i*3))

    def test_get_many(self):
        e = delta_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))

        for i in range(1000, 10000):
            self.assertEqual(e.get(i), int(i*3))

    def test_contains(self):
        e = delta_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))

        for i in range(1000, 10000):
            self.assertTrue(e.contains(int(i*3)))

    def test_get_storage_size(self):
        e = delta_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))

        self.assertEqual(9000*12, e.storage_size())

    def test_range(self):
        e = delta_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))

        last_i = 0
        for i in e.range(5000, 15000, ResultType.VALUE):
            self.assertLess(4999, i)
            self.assertGreater(15001, i)
            self.assertGreater(i, last_i)
            last_i = i

    def test_get_storage_size(self):
        e = delta_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))

        self.assertEqual(87628, e.storage_size())
