import json
import uuid
import io

from eon.query.plan import Plan

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


class Parser:
    def __init__(self, db, data):
        self.db = db
        self.query = data
        self.qid = uuid.uuid4()
        self.msg = io.StringIO()
        self.plan = Plan()

    def _error(self, msg):
        self.msg.write("ERROR: ")
        self.msg.write(msg)

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

        if type(left) is dict:
            if not self._parse_expr(left):
                return False

        if type(right) is dict:
            if not self._parse_expr(right):
                return False



    def _parse_predicate(self, group, is_and=True):
        first = True
        for expr in group:
            if not self._parse_expr(expr):
                return False

            if first:
                first = False
                continue

            # Generate a logical op for the two items on the top of the stack.
            if is_and:
                self.plan.logical_and()
            else:
                self.plan.logical_or()

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

    def get_message(self):
        return self.msg.getvalue()

    def get_plan(self):
        return self.plan

    def compile(self):
        """
        Compiles a JSON query into a query plan.

        :return: The query plan.
        """

        base_table_name = self.query.get("from")
        if base_table_name is None:
            self._error("Unable to parse query, no 'from' clause.")
            return False

        base_table = self.db.get_table(base_table_name)
        if base_table is None:
            self._error("Database does not contain a table named '%s'", base_table_name)
            return False

        if "where" in self.query:
            if not self._parse_where():
                return False

        return True
