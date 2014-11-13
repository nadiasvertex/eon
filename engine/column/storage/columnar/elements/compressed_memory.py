import bisect
import sys

from engine.column.storage.columnar.memory import ResultType
from engine.column.storage import varint
from engine.schema.datatype import DataType


__author__ = 'Christopher Nelson'


class Element:
    def __init__(self, membase, existing_element, compressor_factory, decompressor_factory):
        self.membase = membase
        self.values = bytearray()
        self.rowids = []
        self.offsets = []
        self.df = decompressor_factory
        self.cf = compressor_factory

        if membase.data_type in (DataType.i8, DataType.i16, DataType.i32, DataType.i64):
            self._store_int(existing_element)
        elif membase.data_type in (DataType.f32, DataType.i64):
            self._store_fixed(existing_element)
        else:
            self._store_variable(existing_element)

    def _get_rowids(self, existing_element):
        d = {}
        # First pass, create a sorted array for the rowids.
        for rowid, value in iter(existing_element):
            d[rowid] = value
            self.rowids.append(rowid)
        self.rowids.sort()
        return d

    def _store_int(self, existing_element):
        """
        Stores integers in a very compact format, from which they can easily be recovered.

        :param existing_element: The existing element to fetch the data from.
        """
        d = self._get_rowids(existing_element)

        compacted_values = bytearray()

        # Second pass, store the values in row order.
        for rowid in self.rowids:
            self.offsets.append(len(compacted_values))
            varint.encode_signed(compacted_values.append, d[rowid])

        c = self.cf()

        # Third pass, compress the whole thing.
        self.values.append(c.compress(compacted_values))
        self.values.append(c.flush())


    def _store_fixed(self, existing_element):
        """
        Stores fixed-sized data in an efficient format.

        :param existing_element: The existing element to fetch the data from.
        """
        d = self._get_rowids(existing_element)
        c = self.cf()

        # Second pass, store the values in row order.
        for rowid in self.rowids:
            self.values.append(
                c.compress(bytes(d[rowid]))
            )
        self.values.append(
            c.flush()
        )

    def _store_variable(self, existing_element):
        """
        Stores variable length data as efficiently as possible: each element is
        prefixed with a varint encoded length.

        :param existing_element: The existing element to fetch the data from.
        """
        d = self._get_rowids(existing_element)

        compacted_values = bytearray()

        # Second pass, store the values in row order.
        for rowid in self.rowids:
            v = bytes(d[rowid])
            self.offsets.append(len(compacted_values))
            varint.encode(compacted_values.append, len(v))
            compacted_values.append(v)

        c = self.cf()

        # Third pass, compress the whole thing.
        self.values.append(c.compress(compacted_values))
        self.values.append(c.flush())

    def _enumerate_int(self):
        """
        Provides a generator which will enumerate all of the values in the store. This
        function expects that they are an integer of some kind.

        :return: Each yield provides the index of the item in the system and the value.
        """
        last_value = 0
        for i in range(0, len(self.rowids)):
            value, pos = varint.decode_signed(self.values, pos)
            value += last_value
            last_value = value

            yield (i, value)

    def storage_size(self):
        return sys.getsizeof(self.rowids) + sys.getsizeof(self.values)

    def put(self, rowid, value):
        """
        Puts a new value into the element at the given row.
        :param rowid: The row id where the value should go.
        :param value: The value to store.
        """
        raise RuntimeError("A compressed element is not updateable. Elements can be deleted, but never changed.")

    def get(self, rowid):
        """
        Get the value at the given row id.

        :param rowid: The row to get the value from.
        :return: The value, or None if there is no value stored there.
        """
        row_index = bisect.bisect_left(self.rowids, (rowid, 0))
        if row_index >= len(self.rowids):
            return None

        rid, index = self.rowids[row_index]
        if rid == rowid:
            for i, value in self._enumerate_values():
                if i == index:
                    return value

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
        for index, value in self._enumerate_values():
            if value == value:
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
        for i, value in self._enumerate_values():
            if not predicate(value):
                continue

            yield self._rowid_at_index(i) if want == ResultType.ROW_ID \
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
        if len(self.values) == 0:
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

            yield self._rowid_at_index(index) if want == ResultType.ROW_ID \
                else value
