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
    A data column with fast write properties. It uses a tremendous amount of memory, so after it reaches
    a critical size it should be frozen. Point searches on this data structure are not very fast because
    there are no indexes. That is generally okay because this kind of column should not last very long.
    """

    def __init__(self, data_type):
        self.data_type = data_type
        self.data = []

    def insert(self, value):
        self.data.append(value)
        return len(self.data) - 1

    def freeze(self):
        sorted_data = sorted([(v, i) for i, v in enumerate(self.data)])
        data = [t[0] for t in sorted_data]
        index = [t[1] for t in sorted_data]
        if self.data_type in (DataType.small_int, DataType.standard_int, DataType.big_int):
            return FrozenNumericColumn(self.data_type, data), index

    def get(self, index):
        return self.data[index]

    def get_data(self):
        """
        Provides the column data as an array.
        :return: A numpy array containing the data.
        """
        cons = constructor_map[self.data_type]
        return cons(self.data)

    def vector_join(self, array):
        """
        Join array against this column. Yields up a series of lists of
        tuples.

        :param array: The array to join against.
        :return: A list of tuples encoded as a vstacked array. The local column indexes are on the left, and the
                 array indices are on the right.
        """
        data = np.array(self.data) if type(self.data) is list else self.data
        common = np.intersect1d(array, data)
        for v in common:
            local_indices = np.nonzero(np.in1d(data, v))[0]
            array_indices = np.nonzero(np.in1d(array, v))[0]
            for idx in local_indices:
                local_idx = np.zeros(len(array_indices), dtype="int")
                local_idx.fill(idx)
                yield (local_idx, array_indices)

    def stream_join(self, array):
        for local, remote in self.vector_join(array):
            for i, v in enumerate(local):
                yield (v, remote[i])

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

    def get_data(self):
        """
        Provides the column data as an array.
        :return: A numpy array containing the data.
        """
        return self.data

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
