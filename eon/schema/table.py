from copy import copy
from eon.schema.column import Column
from eon.store.row import Row

__author__ = 'csnelson'


class Table:
    def __init__(self, name=None, columns=[]):
        self.name = name
        self.columns = columns
        self.col_index_map = None
        self.col_map = None

        # Maximum number of rows in a segment.
        self.segment_row_limit = 1 << 16

        self.in_flight = Row(0, self.get_row_type())
        self.segment_rids = []
        self.segments = []

        self._second_stage_init()

    def _second_stage_init(self):
        self.col_index_map = {c.name: i for i, c in enumerate(self.columns)}
        self.col_map = {c.name: c for c in self.columns}

    def _check_nulls(self, present):
        for i, c in enumerate(self.columns):
            if present[i] == False and c.nullable == False:
                return False, "Column '%s' does not allow null values." % c.name
        return True, None

    def get_row_type(self):
        """
        Provides a generator that yields the row type for this table.
        :return: A generator for the row type of this table.
        """
        return (c.data_type for c in self.columns)

    def get(self, rid):
        # Find segment that contains the row identifier.
        if rid < self.in_flight:
            bisect


    def insert(self, data):
        """
        Writes data into the table.
        :param data: The data to write. This is a dictionary where each column name is a key, and the value is the
        data for that column. If the value is a list, then the values specify consecutive rows. Every value must
        have the same amount of data, otherwise data corruption will result.

        :return: A tuple of (False, message) if it failed, (True, rid) if it worked. If data is a bulk load, it will
                 return (True, [rid1, rid2,...])
        """
        if not data:
            return False, "No data to write."

        # Freeze the current in-flight segment if we've hit our limit.
        if len(self.in_flight) >= self.segment_row_limit:
            frozen_segment = self.in_flight.freeze()
            self.segment_rids.append(self.in_flight.base_rid)
            self.segments.append(frozen_segment)
            next_base_rid = self.in_flight.base_rid + self.segment_row_limit
            del self.in_flight
            self.in_flight = Row(next_base_rid, self.get_row_type())

        # Process the write request.
        indexes = [self.col_index_map.get(k) for k, v in data.items() if v is not None]
        if None in indexes:
            column_names = data.keys()
            unknown_names = ["'%s'" % column_names[i]
                             for i, k in enumerate(self.col_index_map.keys())
                             if k is None]
            return False, "Unknown column name(s) %s" % (", ".join(unknown_names))

        # Turn our list of indexes into a present template, so the row will be able to properly
        # compress null values.
        present = [True if i in indexes else False for i in range(0, len(self.columns))]
        okay, msg = self._check_nulls(present)
        if not okay:
            return False, msg

        # Sort the value lists by their index for more efficient insertion.
        sorted_indexes = sorted([(indexes[i], v) for i, v in enumerate(data.values()) if v is not None])

        # Write the data, vector or scalar. We require that all values be the same length, so examining the first
        # value should provide the same results as examining any other.
        if type(sorted_indexes[0][1]) is list:
            # Make sure the values are all the same length.
            lengths = {len(el[1]) for el in sorted_indexes}
            if len(lengths) > 1:
                return False, "Not all value lists are the same length."

            rids = []
            for offset in range(0, lengths.pop()):
                row_present = copy(present)
                row_values = []
                for i, va in sorted_indexes:
                    v = va[offset]
                    # If the value is null, then we don't want to write it to the column. We
                    # flip the present bit to alert the row to this condition, and don't append
                    # the value.
                    if v is None:
                        row_present[i] = False
                    else:
                        row_values.append(v)

                # Check constraints
                okay, msg = self._check_nulls(row_present)
                if not okay:
                    for rid in rids:
                        self.in_flight.delete(rid)
                    return False, msg

                rids.append(self.in_flight.insert(row_present, row_values))

            return True, rids
        else:
            sorted_values = [el[1] for el in sorted_indexes]
            return True, self.in_flight.insert(present, sorted_values)

    def store(self):
        return {
            "name": self.name,
            "columns": [c.store() for c in self.columns],
            "segment_row_limit": self.segment_row_limit
        }

    def load(self, data):
        self.name = data["name"]
        self.columns = [Column().load(cd) for cd in data["columns"]]
        self.segment_row_limit = data["segment_row_limit"]

        self._second_stage_init()
        return self
