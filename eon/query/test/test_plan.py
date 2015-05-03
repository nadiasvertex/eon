__author__ = 'Christopher Nelson'

import random
import unittest

from eon.query.plan import Plan
from eon.schema.data import DataType
from eon.store.row import Row

class PlanTest(unittest.TestCase):
    def test_create(self):
        _ = Plan()
