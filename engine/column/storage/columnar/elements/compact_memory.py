import bisect

from array import array
import sys
from engine.column.storage.columnar.memory import ResultType


__author__ = 'Christopher Nelson'


class Element:
    def __init__(self, membase):
        self.membase = membase
        self.values = array(membase.typecode)
        self.rowids = array('Q')
        self.value_query_count = 0

    def __iter__(self):
        """
        Provides a way to access all of the items in the element.

        :return: A generator over all of the items in the element. Yields a tuple of (rowid, value)
        """
        for i, v in enumerate(self.values):
            yield self.rowids[i], v

    def storage_size(self):
        return sys.getsizeof(self.rowids) + sys.getsizeof(self.values)

    def put(self, rowid, value):
        """
        Puts a new value into the element at the given row.
        :param rowid: The row id where the value should go.
        :param value: The value to store.
        """
        if len(self.rowids) == 0:
            self.rowids.append(rowid)
            self.values.append(value)
            return

        row_index = bisect.bisect_right(self.rowids, rowid)

        self.rowids.insert(row_index, rowid)
        self.values.insert(row_index, value)

    def get(self, rowid):
        """
        Get the value at the given row id.

        :param rowid: The row to get the value from.
        :return: The value, or None if there is no value stored there.
        """
        row_index = bisect.bisect_left(self.rowids, rowid)
        if row_index >= len(self.rowids):
            return None

        if self.rowids[row_index] == rowid:
            return self.values[row_index]

        return None

    def count(self):
        """
        Get the number of rows.

        :return: The number of rows as an integer.
        """
        return len(self.rowids)

    def contains(self, value):
        """
        Discover if this column contains this value.

        :param value: The value to test for.
        :return: True if the value is in this column, false otherwise.
        """
        self.value_query_count += 1
        # TODO: When this value exceeds some threshold:
        # move to a representation that can more efficiently answer these kinds of queries.
        return value in self.values

    def where(self, predicate, want=ResultType.ROW_ID):
        """
        Filter the column using the predicate.

        :param predicate: A callable which evaluates each value in the column. A return value of True cause the value to
                          be selected.
        :param want: Determines if you want row ids or column ids.
        :return: A generator that provides the values for which predicate returns True.
        """
        for i, v in enumerate(self.values):
            if not predicate(v):
                continue

            yield self.rowids[i] if want == ResultType.ROW_ID \
                else v

    def range(self, low, high, want=ResultType.ROW_ID):
        """
        Provides a generator over a range of values. This operation is not very efficient on this element because it
        must copy the value array, index it, and then sort it.

        :param low: The lowest value to return.
        :param high: The highest value to return.
        :param want: Determines if you want row ids or column ids.
        :return: All values that fall in the given range.
        """
        if len(self.values) == 0:
            return

        # Range queries are more expensive than point queries in this system.
        self.value_query_count += 10

        # TODO: Check to see if it is cheaper to create the list and then sort,
        # or to create a sorted list. (This is probably cheaper because append
        # is O(1), and the sort runs in place. Whereas creating a sorted list
        # incurs an O(n) for every insert. Plus you have to start the search
        # from the beginning on every insert.)
        values = [(v, i) for i, v in enumerate(self.values)]
        values.sort()

        value_index = bisect.bisect_left(values, (low, 0))
        for indexed_value in iter(values[value_index:]):
            value, index = indexed_value
            if value < low:
                continue

            if value > high:
                return

            yield self.rowids[index] if want == ResultType.ROW_ID \
                else value
