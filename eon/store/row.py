from copy import deepcopy

from eon.store.column import Column


__author__ = 'Christopher Nelson'


class ImmutableError(Exception):
    def __init__(self):
        Exception.__init__(self)


class Row:
    def __init__(self, base_rid, row_type):
        self.base_rid = base_rid
        self.row_type = row_type
        self.data = []
        self.deleted = []
        self.columns = [Column(data_type) for data_type in row_type]

    def __len__(self):
        return len(self.data)

    def __contains__(self, item):
        return self.base_rid <= item <= self.base_rid + len(self.data)

    def __getitem__(self, item):
        if type(item) is int:
            idx = item - self.base_rid
            if idx > len(self.data):
                raise IndexError()

            return self.get(idx)

    def insert(self, columns, values):
        """
        Inserts the given values into the data store.

        :param columns: A tuple of booleans, one for each attribute in the row_type.
                        If a present value is False, there is no value present for
                        that column in the values tuple.
        :param values: A tuple of values that should be written into the row.
        :return: The row id.
        """
        column_rids = []
        for i, present in enumerate(columns):
            if not present:
                column_rids.append(None)
                continue

            value = values[i]
            column = self.columns[i]
            column_rids.append(column.insert(value))

        self.data.append(column_rids)
        self.deleted.append(False)
        return self.base_rid + len(self.data) - 1

    def join(self, column_no, array):
        column = self.columns[column_no]
        return column.join(array)

    def select(self, columns, idx):
        """
        Get specific column values at the given index.
        :param columns: A list of boolean values for each column in the table. A value of True means to select the
                        column, a value of false means skip it.
        :param idx: The row index to get.
        :return: A list of values for the given columns at the given row index.
        """
        d = self.data[idx]
        r = []
        for i, present in enumerate(columns):
            if not present:
                continue

            row_idx = d[i]
            r.append(self.columns[i].get(row_idx) if row_idx is not None else None)

        return r

    def get(self, idx):
        """
        Get all column values at the given index.
        :param idx: The row index to get.
        :return: A list of values for the given row index.
        """
        d = self.data[idx]
        return [self.columns[i].get(row_idx) if row_idx is not None else None
                for i, row_idx in enumerate(d)]

    def get_column(self, column_no):
        return self.columns[column_no]

    def delete(self, rid):
        idx = rid - self.base_rid
        self.deleted[idx] = True

    def freeze(self):
        # TODO: Eliminate deleted rows during freeze.
        new_columns = []
        new_data = deepcopy(self.data)
        for ci, col in enumerate(self.columns):
            new_col, indices = col.freeze()
            new_columns.append(new_col)
            # We need to remap the row's column indices because a frozen
            # column is generally sorted or otherwise more optimal.
            # Since we do not store a null value in a column the column's
            # indices don't necessarily map 1:1 to the row's. So we have to
            # first index this column.
            index = {}
            ix = 0
            for i, column_rids in enumerate(self.data):
                if column_rids[ci] is not None:
                    index[ix] = i
                    ix += 1

            # Now run through the indices and remap them to the new indices
            # for the column.
            for i, v in enumerate(indices):
                ix = index[v]
                new_data[ix][ci] = i

        return FrozenRow(self.base_rid, self.row_type, new_columns, new_data)


class FrozenRow(Row):
    def __init__(self, base_rid, row_type, columns, data):
        Row.__init__(self, base_rid, row_type)
        self.columns = columns
        self.data = data

    def insert(self, columns, values):
        raise ImmutableError
