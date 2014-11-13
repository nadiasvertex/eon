from engine.io.compressed import Reader

__author__ = 'Christopher Nelson'

import bz2
import unittest


class TestCompressedReader(unittest.TestCase):
    def setUp(self):
        pass

    def test_read(self):
        raw_data = b"Some test data."
        c_data = bz2.compress(raw_data)

        r = Reader(c_data, bz2.BZ2Decompressor())
        for i in range(0, len(raw_data), 4):
            data = r.read(4)
            self.assertEquals(data, raw_data[i:i+4])

    def test_read_overflow(self):
        raw_data = b"Some test data."
        c_data = bz2.compress(raw_data)

        r = Reader(c_data, bz2.BZ2Decompressor())
        self.assertEquals(r.read(1024), raw_data)

