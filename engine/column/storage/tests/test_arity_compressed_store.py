import random
import shutil
import unittest
import tempfile
import struct

from engine.column.storage import arity_compress


__author__ = 'Christopher Nelson'


class TestArityCompressedStore(unittest.TestCase):
    def setUp(self):
        self.table_path = tempfile.mkdtemp()
        self.store = arity_compress.ArityCompressedStore("test_col", self.table_path)

    def tearDown(self):
        shutil.rmtree(self.table_path)

    def test_insert(self):
        with self.store.begin(write=True) as txn:
            txn.put(1, b'test')
            value = txn.get(1)
            self.assertEqual(b'test', bytes(value))


    def test_select(self):
        with self.store.begin(write=True) as txn:
            txn.put(1, b'test')

        with self.store.begin(write=False) as txn:
            value = txn.get(1)
            self.assertEqual(b'test', bytes(value))

    def test_compression(self):
        row_id = 0
        with self.store.begin(write=True) as txn:
            for i in range(0, 100):
                value = b'test' + struct.pack("=B", i)
                for j in range(0, random.randint(1, 10000)):
                    txn.put(row_id, value)
                    row_id += 1

        with self.store.begin(write=False) as txn:
            self.assertEqual(100, len(txn.unique()))
            self.assertEqual(row_id-1, txn.count())




if __name__ == '__main__':
    unittest.main()
