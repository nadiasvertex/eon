__author__ = 'Christopher Nelson'
from enum import Enum

from ds import compute


class ResultStatus(Enum):
    accepted = 0
    rejected = 1
    pending = 2


class RowProcessor:
    def __init__(self, row_id, present, value, predicates, jm):
        self.row_id = row_id
        self.present = present
        self.value = value
        self.predicates = predicates
        self.jm = jm
        self.current_predicate = 0

    def _process(self):
        p = self.predicates[self.current_predicate]
        args = p["args"]
        predicate_op = p["op"]
        if predicate_op == "inner_join":
            column_value = compute.get_value(self.present, self.value, args[0])
            request = (self.row_id, column_value, args[1])
            self.jm.fetch_row_scan(request)
            return ResultStatus.pending
        else:
            op = getattr(compute, predicate_op)
            processed_args = [compute.get_value(self.present,
                                                self.value, arg)
                              for arg in args]
            if not op(*processed_args):
                return ResultStatus.rejected

        return ResultStatus.accepted

    def process(self):
        while True:
            result = self._process()
            if result == ResultStatus.accepted:
                self.current_predicate += 1
                if self.current_predicate >= len(self.predicates):
                    return ResultStatus.accepted

            # Either of the two cases (pending, rejected) need
            # to be handled by the next layer.
            return result


class SetProcessor:
    def __init__(self):
        pass