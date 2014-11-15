import bisect
import sys

from engine.column.storage.columnar.memory import ResultType


__author__ = 'Christopher Nelson'


class Element:
    def __init__(self, membase):
        self.membase = membase
        self.db = {}
        self.values = None
        self.value_query_count = 0
        self.index_threshold = 5000

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
        self.db[rowid] = value
        if self.values is not None:
            bisect.insort(self.values, (value, rowid))

    def get(self, rowid):
        """
        Get the value at the given row id.

        :param rowid: The row to get the value from.
        :return: The value, or None if there is no value stored there.
        """
        return self.db[rowid]

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
