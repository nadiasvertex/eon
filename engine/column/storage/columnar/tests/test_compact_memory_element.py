import tempfile
import unittest

from engine.column.storage.columnar import memory
from engine.column.storage.columnar.elements import compact_memory
from engine.column.storage.columnar.rtype import ResultType
from engine.schema.datatype import DataType


__author__ = 'Christopher Nelson'


class TestElement(unittest.TestCase):
    def setUp(self):
        self.table_path = tempfile.mkdtemp()
        self.mb = memory.Membase(DataType.i32)

    def test_rowid_invariants(self):
        e = compact_memory.Element(self.mb)
        e.put(7, 101)
        self.assertRaises(ValueError, lambda: e.put(5, 70))

    def test_put(self):
        e = compact_memory.Element(self.mb)
        e.put(7, 101)
        self.assertEqual(1, e.count())

    def test_put_many(self):
        e = compact_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))
        self.assertEqual(9000, e.count())

    def test_get(self):
        e = compact_memory.Element(self.mb)
        e.put(7, 101)

        self.assertEqual(101, e.get(7))

    def test_get_some(self):
        e = compact_memory.Element(self.mb)
        for i in range(1000, 1010):
            e.put(i, int(i*3))

        for i in range(1000, 1010, -1):
            self.assertEqual(e.get(i), int(i*3))

    def test_get_many(self):
        e = compact_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))

        for i in range(1000, 10000):
            self.assertEqual(e.get(i), int(i*3))

    def test_contains(self):
        e = compact_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))

        for i in range(1000, 10000):
            self.assertTrue(e.contains(int(i*3)))

    def test_range(self):
        e = compact_memory.Element(self.mb)
        for i in range(1000, 10000):
            e.put(i, int(i*3))

        last_i = 0
        for i in e.range(5000, 15000, ResultType.VALUE):
            self.assertLess(4999, i)
            self.assertGreater(15001, i)
            self.assertGreater(i, last_i)
            last_i = i

    def test_get_storage_size(self):
        e = compact_memory.Element(self.mb)
        for i in range(1000, 100000):
            e.put(i, int(i*3))

        self.assertEqual(408344, e.storage_size())
