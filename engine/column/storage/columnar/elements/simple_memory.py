__author__ = 'Christopher Nelson'

class Element:
    def __init__(self, membase):
        self.membase = membase
        self.db = {}

    def put(self, rowid, value):
        """
        Puts a new value into the element at the given row.
        :param rowid: The row id where the value should go.
        :param value: The value to store.
        """
        self.db[rowid] = value

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
        return value in self.db.values()

    def where(self, predicate, want=ResultType.ROW_ID):
        for k, v in self.db.items():
            if not predicate(v):
                continue

            yield k if want == ResultType.ROW_ID \
                else v

    def range(self, low, high, want=ResultType.ROW_ID):

        value_index = bisect.bisect_left(self.values, low)
        for i, value in enumerate(iter(self.values[value_index:]), value_index):
            if value < low:
                continue

            if value > high:
                return

            yield self.rowids[self.value_to_row[i]] if want == ResultType.ROW_ID \
                else value

