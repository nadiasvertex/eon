import bisect
from heapq import merge
import sys

from engine.column.storage.columnar.rtype import ResultType
from array import array


__author__ = 'Christopher Nelson'


class Element:
    def __init__(self, membase):
        self.membase = membase
        self.db = {}
        self.values = None
        self.levels = array(membase.typecode)
        self.current_level = 0
        self.base_rowid = None
        self.value_query_count = 0
        self.index_threshold = 5000

    def __iter__(self):
        return self.db.items()

    def _create_value_index(self):
        self.values = [(v, r) for r, v in self.db.items()]
        self.values.sort()

    def storage_size(self):
        return sys.getsizeof(self.db) + sys.getsizeof(self.values)

    def put(self, rowid, value):
        """
        Puts a new value into the element at the given row.
        :param rowid: The row id where the value should go.
        :param value: The value to store.
        """
        if self.base_rowid is None:
            self.base_rowid = rowid
        row_index = rowid - self.base_rowid

        required_size = (1 << (self.current_level + 1))
        actual_size = len(self.levels)
        if actual_size < required_size:
            array.extend((0 for i in range(0, required_size - actual_size)))

        # Insert the value by doing an amortized sort over the various levels.
        # Of course, at level 0 no sort is needed. We use a merge sort to construct
        # all following levels.
        if self.current_level == 0:
            self.levels[0] = value
        else:
            sources = [[value]]
            for i in range(0, self.current_level):
                start_index = (1 << i) - 1
                end_index = start_index + (1 << i)
                g = (self.levels[j] for j in range(start_index, end_index + 1))
                sources.append(g)

            target_index_start = (1 << self.current_level) - 1
            for i, k in enumerate(merge(sources), target_index_start):
                self.levels[i] = k

        self.current_level += 1


    def get(self, value):
        """
        Get the rowid at the given value.

        :param value: The value to find the row for.
        :return: The value, or None if there is no value stored there.
        """
        for i in range(0, self.current_level):
            start_index = (1 << i) - 1
            end_index = start_index + (1 << i)

            idx = bisect.bisect_right(self.levels, value, start_index, end_index)


    def count(self):
        return len(self.db)

    def contains(self, value):
        self.value_query_count += 1
        if self.value_query_count >= self.index_threshold and self.values is None:
            self._create_value_index()

        if self.values is None:
            return value in self.db.values()

        value_index = bisect.bisect_left(self.values, (value, 0))
        return self.values[value_index][0] == value

    def where(self, predicate, want=ResultType.ROW_ID):
        for k, v in self.db.items():
            if not predicate(v):
                continue

            yield k if want == ResultType.ROW_ID \
                else v

    def range(self, low, high, want=ResultType.ROW_ID):
        self.value_query_count += 10
        if self.value_query_count >= self.index_threshold and self.values is None:
            self._create_value_index()

        values = self.values if self.values is not None \
            else sorted([(v, r) for r, v in self.db.items()])

        value_index = bisect.bisect_left(values, (low, 0))
        for i, item in enumerate(iter(values[value_index:]), value_index):
            value, rid = item
            if value < low:
                continue

            if value > high:
                return

            yield rid if want == ResultType.ROW_ID \
                else value
