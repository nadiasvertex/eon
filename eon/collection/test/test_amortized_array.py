import bisect
import random
import timeit
import unittest

import numpy as np

from eon.collection.amortized import AmortizedArray


__author__ = 'Christopher Nelson'

s1 = """
from eon.collection.test.test_amortized_array import _test_append_and_sort
import random
a = []
c = random.sample(range(0, 10000), 9000)
"""

s2 = """
from eon.collection.test.test_amortized_array import _test_amortized
from eon.collection.amortized import AmortizedArray
import random
import numpy as np

a = AmortizedArray(np.int)
c = random.sample(range(0, 10000), 9000)
"""

s3 = """
from eon.collection.test.test_amortized_array import _test_insort
import random
a = []
c = random.sample(range(0, 10000), 9000)
"""


def _test_append_and_sort(a, c):
    for v in c:
        a.append(v)
        a.sort()


def _test_insort(a, c):
    for v in c:
        bisect.insort(a, v)


def _test_amortized(a, c):
    for v in c:
        a.append(v)


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
        self.assertEqual(len(d), len(list(a)))
        for i, v in enumerate(a):
            with self.subTest(i=i):
                self.assertEqual(d[i], v)

    def test_performance(self):

        v1 = timeit.timeit("_test_append_and_sort(a,c)", setup=s1, number=10)
        v2 = timeit.timeit("_test_amortized(a,c)", setup=s2, number=10)
        v3 = timeit.timeit("_test_insort(a,c)", setup=s3, number=10)

        self.assertLess(v2, v1)
        self.assertLess(v2, v3)
