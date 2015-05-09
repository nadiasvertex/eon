import random
import unittest

import numpy as np
from eon.collection.amortized import AmortizedArray

__author__ = 'Christopher Nelson'


class AmortizedArrayTest(unittest.TestCase):
    def test_insert(self):
        a = AmortizedArray(np.int)
        c = random.sample(range(0, 10000), 1500)
        for v in c:
            a.append(v)

    def test_iterate(self):
        a = AmortizedArray(np.int)
        c = random.sample(range(0, 10000), 1500)
        for v in c:
            a.append(v)

        d = sorted(c)
        for i, v in enumerate(a):
            with self.subTest(i=i):
                self.assertEqual(d[i], v)

