__author__ = 'Christopher Nelson'

import unittest

from eon.engine.driver.postgres import ConnectionPool
from eon.manager.warm_store import ManagementDriver


class TestConnectionPool(unittest.TestCase):
    def setUp(self):
        self.mgr = ManagementDriver()
        self.database_name = "test_connection_pool_db"
        self.mgr.create(self.database_name)

    def tearDown(self):
        self.mgr.drop(self.database_name)

    def test_connect(self):
        pool = ConnectionPool(self.database_name)

