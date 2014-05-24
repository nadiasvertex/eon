__author__ = 'Christopher Nelson'

import unittest

from eon.engine.driver.postgres import ConnectionPool
from eon.manager.warm_store import ManagementDriver


class TestConnectionPool(unittest.TestCase):
    def setUp(self):
        self.mgr = ManagementDriver()
        self.database_name = "test_connection_pool_db"
        self.user_name = "test_user"
        self.password = "test_password"
        self.mgr.create(self.database_name)
        self.mgr.create_user(self.database_name, self.user_name, self.password)

    def tearDown(self):
        self.mgr.drop_user(self.database_name, self.user_name)
        self.mgr.drop(self.database_name)

    def test_connect(self):
        pool = ConnectionPool(self.database_name, self.user_name, self.password)

        # We only check connections to the local node.
        for i in range(0, 1000):
            with pool.connect(("localhost", 5432)) as conn:
                pass

        # Now try to make several outstanding connections.
        for i in range(0, 1000):
            with pool.connect(("localhost", 5432)) as conn:
                with pool.connect(("localhost", 5432)) as conn2:
                    with pool.connect(("localhost", 5432)) as conn3:
                        with pool.connect(("localhost", 5432)) as conn4:
                            pass
