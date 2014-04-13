__author__ = 'Christopher Nelson'

import unittest

import psycopg2
from eon.manager.warm_store import ManagementDriver

DATABASE_NAME = "test"

class TestWarmStoreManagement(unittest.TestCase):
    def setUp(self):
        self.driver = ManagementDriver()

    def tearDown(self):
        status = self.driver.status(DATABASE_NAME)
        if status is not None:
            self.driver.drop_user(DATABASE_NAME, "test_user")
            self.driver.drop(DATABASE_NAME)

    def test_create_db(self):
        self.driver.create(DATABASE_NAME)
        self.assertIsNotNone(self.driver.status(DATABASE_NAME))

    def test_drop_db(self):
        self.driver.create(DATABASE_NAME)
        self.assertIsNotNone(self.driver.status(DATABASE_NAME))
        self.driver.drop(DATABASE_NAME)
        self.assertIsNone(self.driver.status(DATABASE_NAME))

    def test_create_user(self):
        self.driver.create(DATABASE_NAME)
        self.driver.create_user(DATABASE_NAME, "test_user", "test_password")
        conn = psycopg2.connect(database=DATABASE_NAME,
                                user="test_user",
                                password="test_password",
                                host="localhost")
        conn.close()