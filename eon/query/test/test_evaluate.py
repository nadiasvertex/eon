from eon.schema.column import Column
from eon.schema.data import DataType
from eon.schema.store import Database
from eon.schema.table import Table
from eon.store.row import Row

__author__ = 'Christopher Nelson'

import random
import unittest

import numpy as np

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
        self.sample_1 = c1
        self.sample_2 = c2

    def test_create(self):
        plan = Plan()
        _ = Evaluate(None, plan)

    def test_simple_op(self):
        pivot = random.randint(0, 1 << 16)

        plan = Plan()
        plan.lt(0, pivot)

        e = Evaluate(None, plan)
        selected = e.evaluate(self.full_row)

        c = self.full_row.get_column(0)
        for i, v in enumerate(c.data):
            with self.subTest(i=i):
                self.assertEqual(v < pivot, selected[i])

    def test_binary_op(self):
        pivot1 = random.randint(0, 1 << 16)
        pivot2 = random.randint(0, 1 << 16)

        plan = Plan()
        plan.lt(0, pivot1)
        plan.gt(1, pivot2)
        plan.logical_and()

        e = Evaluate(None, plan)
        selected = e.evaluate(self.full_row)

        c0 = self.full_row.get_column(0)
        c1 = self.full_row.get_column(1)
        for i, v in enumerate(selected):
            with self.subTest(i=i):
                c0_v = c0.data[i]
                c1_v = c1.data[i]
                self.assertEqual(c0_v < pivot1 and c1_v > pivot2, v)

    def test_join_op(self):
        db = Database()
        t1 = Table("test_table_1", columns=[Column("t1_c1", DataType.standard_int)])
        t2 = Table("test_table_2", columns=[Column("t2_c1", DataType.standard_int)])
        db.create_table(t1)
        db.create_table(t2)

        t1.insert({"t1_c1": self.sample_1})
        t2.insert({"t2_c1": self.sample_2})

        plan = Plan()
        plan.join(0, "test_table_2", 0)

        e = Evaluate(db, plan)
        selected = e.evaluate(t1.in_flight)
        intersection = np.intersect1d(np.array(self.sample_1, dtype=np.int), np.array(self.sample_2, dtype=np.int))
        if len(intersection) > 0:
            self.assertIsNotNone(selected)
            self.assertGreaterEqual(len(selected), len(intersection))
