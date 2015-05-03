__author__ = 'Christopher Nelson'

import numpy as np
from eon.query.plan import Op

class Evaluate:
    def __init__(self, row, plan):
        self.row = row
        self.plan = plan
        self.stack = []

    def eval(self):
        for instruction in self.plan.program:
            op = instruction[0]
            if op in (Op.lt, Op.gt):
                column_no = op[1]
                value = op[2]



