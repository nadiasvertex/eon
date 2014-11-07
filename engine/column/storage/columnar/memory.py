import bisect
from array import array

__author__ = 'Christopher Nelson'


class ResultType:
    ROW_ID = 0
    VALUE = 1


class Element:
    def __init__(self, membase):
        self.membase = membase
        self.values = array(membase.typecode)
        self.rowids = array('Q')
        self.row_to_value = array('H')
        self.value_to_row = array('H')

    def put(self, rowid, value):
        """
        Puts a new value into the element at the given row.
        :param rowid: The row id where the value should go.
        :param value: The value to store.
        """
        row_index = bisect.insort(self.rowids, rowid)
        value_index = bisect.insort(self.values, value)
        self.row_to_value[row_index] = value_index
        self.value_to_row[value_index] = row_index

    def get(self, rowid):
        """
        Get the value at the given row id.

        :param rowid: The row to get the value from.
        :return: The value, or None if there is no value stored there.
        """
        row_index = bisect.bisect(self.rowids)
        if self.rowids[row_index] == rowid:
            value_index = self.row_to_value[row_index]
            return self.values[value_index]

        return None

    def contains(self, value):
        value_index = bisect.bisect(self.values, value)
        if value_index == 0 or value_index == len(self.values):
            return False

        return self.values[value_index - 1] == value

    def where(self, predicate, want=ResultType.ROW_ID):
        for i, v in enumerate(self.values):
            if not predicate(v):
                continue

            yield self.rowids[self.values_to_row[i]] if want == ResultType.ROW_ID \
                else v

    def range(self, low, high, want=ResultType.ROW_ID):
        if len(self.values) == 0:
            return

        value_index = bisect.bisect_left(self.values, low)
        for i, value in enumerate(iter(self.values[value_index:]), value_index):
            if value < low:
                continue

            if value > high:
                return

            yield self.rowids[self.value_to_row[i]] if want == ResultType.ROW_ID \
                else value


class Membase:
    def __init__(self, typecode):
        self.typecode = typecode
        self.element_limit = 5000
