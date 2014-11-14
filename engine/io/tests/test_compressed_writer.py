import lzma
from engine.io.compressed import Reader, Writer

__author__ = 'Christopher Nelson'

import bz2
import unittest


class TestCompressedWriter(unittest.TestCase):
    def setUp(self):
        pass

    def test_writer_bz2(self):
        raw_data = b"Some test data."
        c_data = bz2.compress(raw_data)

        w = Writer(bz2.BZ2Compressor())
        w.write(raw_data)

        self.assertEquals(c_data, w.get_bytes())

    def test_writer_lzma(self):
        raw_data = b"Some test data."
        c_data = lzma.compress(raw_data)

        w = Writer(lzma.LZMACompressor())
        w.write(raw_data)

        self.assertEquals(c_data, w.get_bytes())


