__author__ = 'Christopher Nelson'

import numpy as np

from eon.query.plan import Op


class Evaluate:
    def __init__(self, plan):
        self.plan = plan
        self.stack = []

    def evaluate(self, row):
        """
        Evaluates the program against the row.
        :param row: The row to evaluate the program against.
        :return: A boolean vector with a True value in every index that should be selected.
        """
        self.stack.clear()
        for instruction in self.plan.program:
            op = instruction[0]

            # If the op is a simple boolean operation, run it against
            # the column specified. Save the result (a bool vector)
            # in the stack.
            if op in (Op.lt, Op.gt, Op.lte, Op.gte, Op.eq, Op.ne):
                column_no = instruction[1]
                value = instruction[2]
                column = row.get_column(column_no)
                self.stack.append(column.vector_op(op, value))

            # Pop the last two items off the stack, and perform a vector
            # truth operation. Store the result on the stack.
            elif op in (Op.logical_and, Op.logical_or):
                l = self.stack.pop()
                r = self.stack.pop()
                if op == Op.logical_and:
                    self.stack.append(np.logical_and(l, r))
                else:
                    self.stack.append(np.logical_or(l, r))

            # Pop the last item off the stack and perform a logical not, then push
            # the result back on the stack.
            elif op == Op.logical_not:
                d = self.stack.pop()
                self.stack.append(np.logical_not(d))

        return self.stack[-1]



