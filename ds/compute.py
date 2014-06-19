__author__ = 'Christopher Nelson'


def get_column_value(present, values, column_index):
    if column_index not in present:
        return  None

    return values[present.index(column_index)]

def get_value(present, data, arg):
    if arg[0] == 1:
        return arg[1]

    if arg[0] == 0:
        column_idx = arg[1]
        return get_column_value(present, data, column_idx)

    return None


def lt(left, right):
    return left < right
