import json
import uuid

from eon.query.plan import Plan


__author__ = 'Christopher Nelson'

"""
The JSON query is quite simple. It looks like a deconstructed SQL query, because that's what it is.

{
  "from": "tablename",
  "join": [
    {"left":"tablename",
     "right":"tablename",
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
     "column": "new_column_name",
     "store_expr": { ... expression to evaluate when where is true, result is stored in column in output stream ... }
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
    def __init__(self, data):
        self.query = json.loads(data)
        self.qid = uuid.uuid4()


    def compile(self):
        """
        Compiles a JSON query into a query plan.

        :return: The query plan.
        """
        plan = Plan()



