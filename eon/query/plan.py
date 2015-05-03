import enum

__author__ = 'Christopher Nelson'


class Op(enum.Enum):
    lt = 1
    lte = 2
    gt = 3
    gte = 4
    eq = 5
    ne = 6
    logical_and = 7
    logical_or = 8
    logical_not = 9

class Plan:
    def __init__(self):
        self.program = []

    def lt(self, column_no, value):
        self.program.append((Op.lt, column_no, value))

    def gt(self, column_no, value):
        self.program.append((Op.gt, column_no, value))
