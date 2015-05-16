__author__ = 'Christopher Nelson'

import numpy as np

from eon.query.plan import Op


class Evaluate:
    def __init__(self, db, plan):
        self.plan = plan
        self.stack = []
        self.db = db
        self.joins = []

    def evaluate(self, row):
        """
        Evaluates the program against the row.
        :param row: The row to evaluate the program against.
        :return: A boolean vector with a True value in every index that should be selected.
        """

        # Note -  Since we don't store null values, there is no
        # guarantee that the columns will match up with each other. So what we need to do is
        # perform the vector operation, and then if the column length is shorter than the row
        # segment size we need to go through and expand the column segment so that it matches
        # the length of the row. For most of the operations below this is easy to do. A null
        # value simply yields an unselected row.
        #
        # This is implemented by calling row.expand_selection(). If the column is the right length
        # it's just returned. Otherwise it will be expanded as needed.

        self.stack.clear()
        self.joins.clear()
        for instruction in self.plan.program:
            op = instruction[0]

            # If the op is a simple boolean operation, run it against
            # the column specified. Save the result (a bool vector)
            # in the stack.
            if op in (Op.lt, Op.gt, Op.lte, Op.gte, Op.eq, Op.ne):
                column_no = instruction[1]
                value = instruction[2]
                column = row.get_column(column_no)
                self.stack.append(row.get_expanded_selection(column_no, column.vector_op(op, value)))

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

            elif op == Op.join:
                column_no = instruction[1]
                join_table_name = instruction[2]
                join_column_no = instruction[3]

                array = row.get_column(column_no).get_data()
                table = self.db.get_table(join_table_name)
                row_ids = []
                selected_indexes = []
                for rid, array_idx in table.join(join_column_no, array):
                    row_ids.append(rid)
                    selected_indexes.append(array_idx)

                if not row_ids:
                    self.stack.append(row.get_empty_selection())
                    return

                row_id_array = np.concatenate(row_ids) if len(row_ids) > 1 else row_ids[0]
                selected_index_array = np.concatenate(selected_indexes) if len(selected_indexes) > 1 else selected_indexes[0]

                # We now have two arrays. One of them contains all of the
                # indices from the column that can be selected. We want to turn
                # this into a boolean array just like the ones we get from other
                # selection ops above.
                selected = np.zeros(len(array))
                selected[selected_index_array] = True

                # Push the selection mask onto the stack, and keep around the join data.
                self.stack.append(row.get_expanded_selection(column_no, selected))
                self.joins.append((column_no, row_id_array, selected_index_array))

        return self.stack[-1]
