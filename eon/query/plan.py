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
    join = 10


class Plan:
    def __init__(self):
        self.program = []

    def lt(self, column_no, value):
        self.program.append((Op.lt, column_no, value))

    def gt(self, column_no, value):
        self.program.append((Op.gt, column_no, value))

    def lte(self, column_no, value):
        self.program.append((Op.lte, column_no, value))

    def gte(self, column_no, value):
        self.program.append((Op.gte, column_no, value))

    def eq(self, column_no, value):
        self.program.append((Op.eq, column_no, value))

    def ne(self, column_no, value):
        self.program.append((Op.ne, column_no, value))

    def logical_and(self):
        self.program.append((Op.logical_and,))

    def logical_or(self):
        self.program.append((Op.logical_or,))

    def logical_not(self):
        self.program.append((Op.logical_not,))

    def join(self, column_no, join_table_name, join_column_no):
        self.program.append((Op.join, column_no, join_table_name, join_column_no))

