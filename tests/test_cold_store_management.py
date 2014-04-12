__author__ = 'Christopher Nelson'

import unittest
from eon.manager.cold_store import ManagementDriver

class TestColdStoreManagement(unittest.TestCase):
    def setUp(self):
        self.driver = ManagementDriver()

    def tearDown(self):
        status = self.driver.status("test")
        if status is not None:
            self.driver.drop("test")

    def test_create_db(self):
        self.driver.create("test")
        self.assertIsNotNone(self.driver.status("test"))

    def test_drop_db(self):
        self.driver.create("test")
        self.assertIsNotNone(self.driver.status("test"))
        self.driver.drop("test")
        self.assertIsNone(self.driver.status("test"))
