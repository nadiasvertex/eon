from copy import deepcopy

__author__ = 'Christopher Nelson'


class ImmutableError(Exception):
    def __init__(self):
        Exception.__init__(self)


class Row:
    def __init__(self, base_rid, row_type):
        self.base_rid = base_rid
        self.row_type = row_type
        self.data = []
        self.columns = []

    def insert(self, columns, values):
        """
        Inserts the given values into the data store.

        :param columns: A tuple of booleans, one for each attribute in the row_type.= v
                        If a present value is False, there is no value present for
                        that column in the values tuple.
        :param values: A tuple of values that should be written into the row.
        :return: The row id.
        """
        value_idx = 0
        column_rids = []
        for i, present in enumerate(columns):
            if not present:
                column_rids.append(None)
                continue

            value = values[value_idx]
            column = self.columns[value_idx]
            column_rids.append(column.insert(value))

            value_idx += 1

        self.data.append(column_rids)

    def freeze(self):
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

    def get_column(self, column_no):
        return self.columns[column_no]
