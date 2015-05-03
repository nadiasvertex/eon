from eon.schema.data import DataType
from eon.store.row import Row

__author__ = 'Christopher Nelson'

import random
import unittest

from eon.query.plan import Plan
from eon.query.evaluate import Evaluate


class EvaluateTest(unittest.TestCase):
    def setUp(self):
        row_type = (DataType.big_int, DataType.standard_int, DataType.small_int)
        r = Row(0, row_type)

        c1 = random.sample(range(1, 10000), 1800)
        c2 = random.sample(range(1, 10000), 1800)
        c3 = random.sample(range(1, 10000), 1800)

        for i in range(0, len(c1)):
            r.insert((True, True, True), (c1[i], c2[i], c3[i]))

        self.row_type = row_type
        self.full_row = r

    def test_create(self):
        plan = Plan()
        _ = Evaluate(plan)

    def test_simple_op(self):
        pivot = random.randint(0, 1 << 16)

        plan = Plan()
        plan.lt(0, pivot)

        e = Evaluate(plan)
        selected = e.evaluate(self.full_row)

        c = self.full_row.get_column(0)
        for i, v in enumerate(c.data):
            with self.subTest(i=i):
                self.assertEqual(v < pivot, selected[i])

