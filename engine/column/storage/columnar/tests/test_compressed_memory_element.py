import tempfile
import unittest
import zlib

from engine.column.storage.columnar import memory
from engine.column.storage.columnar.elements import compact_memory, compressed_memory
from engine.column.storage.columnar.rtype import ResultType
from engine.schema.datatype import DataType


__author__ = 'Christopher Nelson'


class TestElement(unittest.TestCase):
    def setUp(self):
        self.table_path = tempfile.mkdtemp()
        self.mb = memory.Membase(DataType.i32)

    def test_get(self):
        eb = compact_memory.Element(self.mb)
        eb.put(7, 101)

        e = compressed_memory.Element(self.mb, eb, zlib.compressobj, zlib.decompressobj)
        self.assertEqual(101, e.get(7))

    def test_get_some(self):
        eb = compact_memory.Element(self.mb)
        for i in range(1000, 1010):
            eb.put(i, int(i * 3))

        e = compressed_memory.Element(self.mb, eb, zlib.compressobj, zlib.decompressobj)
        for i in range(1000, 1010):
            self.assertEqual(e.get(i), int(i * 3))

    def test_get_many(self):
        eb = compact_memory.Element(self.mb)
        for i in range(1000, 10000):
            eb.put(i, int(i * 3))

        e = compressed_memory.Element(self.mb, eb, zlib.compressobj, zlib.decompressobj)
        for i in range(1000, 10000):
            self.assertEqual(e.get(i), int(i * 3))

    def test_contains(self):
        eb = compact_memory.Element(self.mb)
        for i in range(1000, 10000):
            eb.put(i, int(i * 3))

        e = compressed_memory.Element(self.mb, eb, zlib.compressobj, zlib.decompressobj)
        for i in range(1000, 10000):
            self.assertTrue(e.contains(int(i * 3)))

    def test_range(self):
        eb = compact_memory.Element(self.mb)
        for i in range(1000, 10000):
            eb.put(i, int(i * 3))

        e = compressed_memory.Element(self.mb, eb, zlib.compressobj, zlib.decompressobj)
        last_i = 0
        for i in e.range(5000, 15000, ResultType.VALUE):
            self.assertLess(4999, i)
            self.assertGreater(15001, i)
            self.assertGreater(i, last_i)
            last_i = i

    def test_get_storage_size(self):
        eb = compact_memory.Element(self.mb)
        for i in range(1000, 100000):
            eb.put(i, int(i * 3))

        e = compressed_memory.Element(self.mb, eb, zlib.compressobj, zlib.decompressobj)
        self.assertEqual(222208, e.storage_size())
