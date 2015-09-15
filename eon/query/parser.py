import json
import subprocess
import tempfile
import uuid
import io

from eon.schema.data import DataType

__author__ = 'Christopher Nelson'

"""
The JSON query is quite simple. It looks like a deconstructed SQL query, because that's what it is.

{
  "from": "tablename",
  "join": [
    {
     "table":"tablename",
     "type":"left|right|cross",
     "outer": false,
     "on": {
        "op":"=", "left":"column", "right":"column",
        # or
        "all": [ ... # like where below ],
        "any": [ ... # like where below ]
     }
  ],
  "select": [
    "column_name",
    {"fn": "add", "left":"column_or_expr", "right":"column_or_expr"},
    {"fn": "subquery", "query":{ ... }},
  ],
  "where":{
    "all": [
        {"op":"<", "left":"column_or_expr", "right":"column_or_expr"},
        "any" : [
            ...
        ]
    ],
    "any": [
        {"op":"<", "left":"column_or_expr", "right":"column_or_expr"}
        "all" : [
          ...
        ]
    }
  ],
  "order-by": [
    "column",
    { ... #expression }
  ],
  "group-by" : [
    "column",
    { ... #expression }
  ],
  "limit":max_rows,
  "offset:rows_to_skip,

  "stream":[
    {"window":5,
     "where": { ... just like where above ... },
     "action": "drop|store_true|store_false|store",
     "store_expr": { ... expression to evaluate when where is true, result is stored in new column in output stream ... }
     "column": "new_column_name",
    }, etc...
  ]
}

An expression is an object with an "op", and whatever operands the op need. For most binary operations this is two
fields "left" and "right". For other operations there are other field names. For example, "subquery" requires an
operand called "query".

Expressions can be used any place a column can be used, but that doesn't mean it is always wise or efficient to
use arbitrarily complex expressions all over the place.

You may also specify stream processing instructions. These are a post-process after the initial query phase, and
allow you to make stream-based transformations of the data before it gets to the client. Stream transforms can
drop data and insert data into the stream based on simple predicates and generators.
"""

op_map = {
    "<": " .lt. ",
    ">": " .gt. ",
    "<=": " .le. ",
    ">=": " .ge. ",
    "<>": " .ne. ",
    "=": " .eq. "
}

data_type_map = {
    DataType.small_int: "integer(1)",
    DataType.standard_int: "integer(2)",
    DataType.big_int: "integer(4)",
    DataType.decimal: "integer(4)",
    DataType.bool: "logical(1)",
}


class Parser:
    def __init__(self, db, data):
        self.db = db
        self.query = data
        self.qid = uuid.uuid4()
        self.msg = io.StringIO()
        self.program = io.StringIO()

        self.runnable_path = None

        self.base_table = None
        self.expr_stack = []
        self.array_map = {}
        self.next_array = 0

    def _error(self, msg):
        self.msg.write("ERROR: ")
        self.msg.write(msg)

    def _allocate_array(self, col_ref, column):
        self.array_map[col_ref] = (
            "a%02d" % self.next_array,
            data_type_map[column.data_type]
        )
        self.next_array += 1

    def _column_reference(self, col_ref):
        parts = col_ref.split(".")
        if len(parts) == 1:
            column = self.base_table.columns[self.base_table.get_column_no(parts[0])]

        self._allocate_array(col_ref, column)
        arr_name, _ = self.array_map[col_ref]
        self.expr_stack.append(arr_name)

        return True

    def _parse_expr_element(self, el):
        if type(el) is dict:
            if not self._parse_expr(el):
                return False
        elif type(el) is str:
            if not self._column_reference(el):
                return False
        elif type(el) is int:
            self.expr_stack.append(str(el))
        else:
            return False

        return True

    def _parse_expr(self, e):
        op = e.get("op")
        if op is None:
            self._error("No operation specified in expression '%s'." % json.dumps(e))
            return False

        left = e.get("left")
        right = e.get("right")

        if left is None:
            self._error(self._error("No left side specified in expression '%s'." % json.dumps(e)))
            return False

        if right is None:
            self._error(self._error("No right side specified in expression '%s'." % json.dumps(e)))
            return False

        if not self._parse_expr_element(left):
            return False
        if not self._parse_expr_element(right):
            return False

        if len(self.expr_stack) > 1:
            r = self.expr_stack.pop()
            l = self.expr_stack.pop()
            self.expr_stack.append("%s%s%s" % (l, op_map[op], r))

        return True

    def _parse_predicate(self, group, is_and=True):
        for expr in group:
            if not self._parse_expr(expr):
                return False

            if len(self.expr_stack) < 2:
                continue

            r = self.expr_stack.pop()
            l = self.expr_stack.pop()

            # Generate a logical op for the two items on the top of the stack.
            if is_and:
                self.expr_stack.append("%s .and. %s" % (l, r))
            else:
                self.program.write(self.expr_stack.append("%s .or. %s" % (l, r)))

        return True

    def _parse_where(self):
        predicate = self.query["where"]
        any_group = predicate.get("any")
        all_group = predicate.get("all")

        if any_group is not None and all_group is not None:
            self._error("WHERE expression may not contain both 'all' and 'any' at the top level. Use nesting "
                        "to provide parenthetical scoping.")
            return False

        if any_group is None and all_group is None:
            self._error("WHERE expression must contain either 'all' or 'any' at the top level.")
            return False

        if any_group is not None:
            return self._parse_predicate(any_group, is_and=False)
        else:
            return self._parse_predicate(all_group)

    def _generate_arrays(self):
        arrays = sorted(self.array_map.keys())
        self.program.write("logical(1), dimension(:), allocatable :: selector\n")
        for k in arrays:
            arr_name, arr_type = self.array_map[k]
            self.program.write(
                "%s, dimension(:), allocatable :: %s\t! %s\n" % (arr_type, arr_name, k)
            )

        self.program.write("\n")

    def _generate_load_arrays(self):
        arrays = sorted(self.array_map.keys())

        self.program.write("integer(4) :: record_count = 0\n")
        self.program.write("read *, record_count\n\n")

        self.program.write("allocate(selector(record_count))\n")
        self.program.write("selector = .true.\n\n")

        for k in arrays:
            arr_name, arr_type = self.array_map[k]
            self.program.write(
                "allocate(%s(record_count)) ! %s\n" % (arr_name, k)
            )
            self.program.write(
                "read *, %s\n" % arr_name
            )

        self.program.write("\n")

    def _compile_query(self):
        with tempfile.NamedTemporaryFile("wt", encoding="utf-8", suffix=".f08") as o:
            o.write(self.get_program())
            o.flush()

            try:
                self.runnable_path = o.name + ".run"
                subprocess.check_output(
                    ["gfortran", "-o", self.runnable_path, o.name],
                    stderr=subprocess.STDOUT
                )
            except subprocess.CalledProcessError as e:
                self._error("INTERNAL: unable to compile query object: " + e.output)
                return False

        return True

    def get_message(self):
        return self.msg.getvalue()

    def get_program(self):
        return self.program.getvalue()

    def compile(self):
        """
        Compiles a JSON query into a query plan.

        :return: The query plan.
        """

        self.program.write("program query_")
        self.program.write(self.qid.hex)
        self.program.write("\n\n")

        base_table_name = self.query.get("from")
        if base_table_name is None:
            self._error("Unable to parse query, no 'from' clause.")
            return False

        self.base_table = self.db.get_table(base_table_name)
        if self.base_table is None:
            self._error("Database does not contain a table named '%s'" % base_table_name)
            return False

        if "where" in self.query:
            if not self._parse_where():
                return False

        # The expression stack should now have a series of boolean expressions that need to be
        # evaluated. Turn those into consecutive where clauses. Also generate the array allocations
        # that need to exist before the execution of the predicates.
        self._generate_arrays()
        self._generate_load_arrays()

        # A number of column arrays may be provided and processed. However, these arrays will always
        # correspond to the same row in the base table. The selector array provides a vector of
        # true or false values indicating whether that row should be selected.
        for expr in self.expr_stack:
            self.program.write("where (selector .and. .not. (")
            self.program.write(expr)
            self.program.write("))\n")
            self.program.write("\tselector = .false.\n")
            self.program.write("end where\n\n")

        self.program.write("print *, record_count\n")
        self.program.write("print *, selector\n")
        self.program.write("end program\n")

        return self._compile_query()
