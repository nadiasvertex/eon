__author__ = 'Christopher Nelson'

import io
import struct
from enum import Enum

import msgpack

from ds import compute, remote


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

    def resume(self):
        self.current_predicate += 1
        return self.process()


class SetProcessor:
    def __init__(self, txn, predicates, jm):
        self.txn = txn
        self.predicates = predicates
        self.jm = jm

        self.deferred = {}
        self.accepted = []

    def _process_deferred(self):
        while True:
            msg = self.jm.get_response()
            # No message, no work.
            if msg is None:
                return

            status, row_id = msg
            # If this row was not found, then we need to delete the deferred processor from our
            # dictionary. Clearly we won't be selecting that row.
            if status == remote.REP_NOT_FOUND:
                del self.deferred[row_id]

            # The row was found, initiate deferred processing.
            rp = self.deferred[row_id]
            del self.deferred[row_id]
            
            self._process_results(rp, rp.resume())

    def _process_results(self, rp, results):
        row_id = rp.row_id
        if results == ResultStatus.rejected:
            return
        if results == ResultStatus.accepted:
            self.accepted.append(row_id)
        elif results == ResultStatus.pending:
            self.deferred[row_id] = rp

    def process(self):
        cursor = self.txn.cursor()
        for item in iter(cursor):
            # Check to see if any of our deferred rows have received their last value yet.
            if len(self.deferred) > 0:
                self._process_deferred()

            # Proceed with processing this row.
            row_id = struct.unpack_from("Q", cursor.key())

            # Use a streaming unpacker to avoid having to copy
            # bytes from the database.
            u = msgpack.Unpacker(io.BytesIO(cursor.value()))
            data = u.unpack()

            # Get the present index for this row, and the stored
            # data.
            present = data[0]
            value = data[1]

            rp = RowProcessor(row_id, present, value, self.predicates, self.jm)
            self._process_results(rp, rp.process())

