__author__ = 'Christopher Nelson'


def get_value(present, data, arg):
    if arg[0] == 1:
        return arg[1]

    if arg[0] == 0:
        column_idx = arg[1]
        if column_idx not in present:
            return None
        data_idx = present.index(column_idx)
        return data[data_idx]

    return None


def lt(left, right):
    return left < right
