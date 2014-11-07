import tempfile
import unittest
from engine.column.storage.columnar import memory

__author__ = 'Christopher Nelson'


class TestMembase(unittest.TestCase):
    def setUp(self):
        self.table_path = tempfile.mkdtemp()

    def test_create(self):
        mb = memory.Membase("i")
        self.assertIsNotNone(mb)

