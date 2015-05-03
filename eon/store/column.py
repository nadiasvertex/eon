import numpy as np

from eon.schema.data import DataType
from eon.query.plan import Op

constructor_map = {
    DataType.bool: np.bool_,
    DataType.small_int: np.int8,
    DataType.standard_int: np.int32,
    DataType.big_int: np.int64
}


class Column:
    """
    A data column with fast write properties. It uses a tremendous amount of memory, so after it reachest is
    a critical size it should be frozen. Point searches on this data structure are not very fast because
    there are no indexes. That is generally okay because this kind of column should not last very long.
    """

    def __init__(self, data_type):
        self.data_type = data_type
        self.data = []

    def insert(self, value):
        self.data.append(value)
        return len(self.data)

    def freeze(self):
        sorted_data = sorted([(v, i) for i, v in enumerate(self.data)])
        data = [t[0] for t in sorted_data]
        index = [t[1] for t in sorted_data]
        if self.data_type in (DataType.small_int, DataType.standard_int, DataType.big_int):
            return FrozenNumericColumn(self.data_type, data), index

    def get(self, index):
        return self.data[index]

    def vector_op(self, op, value):
        """
        Performs the '<' on this column, returning an array of truth values that indicates
        the result of the test for each row in the column.
        :param value: The value to test.
        :return: An array of truth values with the same length as this column.
        """
        if op == Op.lt:
            return np.bool_([v < value for v in self.data])
        elif op == Op.gt:
            return np.bool_([v > value for v in self.data])
        elif op == Op.lte:
            return np.bool_([v <= value for v in self.data])
        elif op == Op.gte:
            return np.bool_([v >= value for v in self.data])
        elif op == Op.eq:
            return np.bool_([v == value for v in self.data])
        elif op == Op.ne:
            return np.bool_([v != value for v in self.data])

    def stream_op(self, op, value):
        """
        Provides a generator which yields the indices for which this operation is true.
        :param value: The value to test.
        :return: A generator yielding the indices for which this operation is true.
        """
        r = self.vector_op(op, value)
        for i, v in enumerate(r):
            if v:
                yield i


class FrozenNumericColumn(Column):
    def __init__(self, data_type, data):
        Column.__init__(self, data_type)
        self.cons = constructor_map[data_type]
        self.data = self.cons(data)

    def insert(self, value):
        raise ValueError("Cannot insert data into a frozen column.")

    def vector_op(self, op, value):
        """
        Performs the specified operation on this column, returning an array of truth values that indicates
        the result of the test for each row in the column.
        :param op: The operation to perform.
        :param value: The value to test.
        :return: An array of truth values with the same length as this column.
        """
        rvalue = self.cons(value)
        if op == Op.lt:
            return np.less(self.data, rvalue)
        elif op == Op.gt:
            return np.greater(self.data, rvalue)
        elif op == Op.lte:
            return np.less_equal(self.data, rvalue)
        elif op == Op.gte:
            return np.greater_equal(self.data, rvalue)
        elif op == Op.eq:
            return np.equal(self.data, rvalue)
        elif op == Op.ne:
            return np.not_equal(self.data, rvalue)
