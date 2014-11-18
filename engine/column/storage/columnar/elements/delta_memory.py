import bisect
import sys

from engine.column.storage.columnar.memory import ResultType
from engine.column.storage import varint


__author__ = 'Christopher Nelson'


class Element:
    def __init__(self, membase):
        self.membase = membase
        self.values = bytearray()
        self.base_rowid = None
        self.row_count = 0
        self.value_query_count = 0
        self.last_value = 0

    def __iter__(self):
        """
        Provides a way to access all of the items in the element.

        :return: A generator over all of the items in the element. Yields a tuple of (rowid, value)
        """
        for i, v in self._enumerate_values():
            yield self.base_rowid + i, v

    def _enumerate_values(self):
        """
        Provides a generator which will enumerate all of the values in the store.
        :return: Each yield provides the index of the item in the system and the value.
        """
        pos = 0
        last_value = 0
        for i in range(0, self.row_count):
            value, pos = varint.decode_signed(self.values, pos)
            value += last_value
            last_value = value

            yield (i, value)

    def storage_size(self):
        return sys.getsizeof(self.values)

    def put(self, rowid, value):
        """
        Puts a new value into the element at the given row.
        :param rowid: The row id where the value should go.
        :param value: The value to store.
        """
        if self.base_rowid is None:
            self.base_rowid = rowid
        elif rowid < self.base_rowid:
            raise ValueError("Row ids must be monotonically increasing.")

        varint.encode_signed(self.values.append, value - self.last_value)
        self.last_value = value
        self.row_count += 1

    def get(self, rowid):
        """
        Get the value at the given row id.

        :param rowid: The row to get the value from.
        :return: The value, or None if there is no value stored there.
        """
        index = rowid - self.base_rowid

        if self.row_count == 0 or index < 0:
            return None

        for i, value in self._enumerate_values():
            if i == index:
                return value

    def count(self):
        """
        Get the number of rows.

        :return: The number of rows as an integer.
        """
        return self.row_count

    def contains(self, value):
        """
        Discover if this column contains this value.

        :param value: The value to test for.
        :return: True if the value is in this column, false otherwise.
        """
        self.value_query_count += 1
        # TODO: When this value exceeds some threshold:
        # move to a representation that can more efficiently answer these kinds of queries.
        for _, stored_value in self._enumerate_values():
            if value == stored_value:
                return True
        else:
            return False

    def where(self, predicate, want=ResultType.ROW_ID):
        """
        Filter the column using the predicate.

        :param predicate: A callable which evaluates each value in the column. A return value of True cause the value to
                          be selected.
        :param want: Determines if you want row ids or column ids.
        :return: A generator that provides the values for which predicate returns True.
        """
        for index, value in self._enumerate_values():
            if not predicate(value):
                continue

            yield self.base_rowid + index if want == ResultType.ROW_ID \
                else value

    def range(self, low, high, want=ResultType.ROW_ID):
        """
        Provides a generator over a range of values. This operation is not very efficient on this element because it
        must copy the value array, index it, and then sort it.

        :param low: The lowest value to return.
        :param high: The highest value to return.
        :param want: Determines if you want row ids or column ids.
        :return: All values that fall in the given range.
        """
        if self.row_count == 0:
            return

        # Range queries are more expensive than point queries in this system.
        self.value_query_count += 10

        # TODO: Check to see if it is cheaper to create the list and then sort,
        # or to create a sorted list. (This is probably cheaper because append
        # is O(1), and the sort runs in place. Whereas creating a sorted list
        # incurs an O(n) for every insert. Plus you have to start the search
        # from the beginning on every insert.)
        values = [(v, i) for i, v in self._enumerate_values()]
        values.sort()

        value_index = bisect.bisect_left(values, (low, 0))
        for indexed_value in iter(values[value_index:]):
            value, index = indexed_value
            if value < low:
                continue

            if value > high:
                return

            yield self.base_rowid + index if want == ResultType.ROW_ID \
                else value
