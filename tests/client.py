from pprint import pprint
from eon.client.data import DataType

__author__ = 'Christopher Nelson'

from eon.client import cluster
from eon.client.exception import DataDefinitionLanguageError

with cluster.connect() as c:
    print("Current databases:")
    pprint(c.databases())

    try:
        db = c.open_database("test")
    except DataDefinitionLanguageError:
        db = c.create_database("test")

    pprint(db.schema)

    if not db.has_table("table_1"):
        db.create_table("table_1", [
            {"name": "id", "data-type": DataType.big_int},
            {"name": "title", "data-type": DataType.varchar},
            {"name": "key_symbol", "data-type": DataType.varchar},
            {"name": "file_size", "data-type": DataType.standard_int}
        ])

    # Test inserting data
    data = [
        {"title": "The Grandest Thing Ever", "file_size": 1500},
        {"title": "Super Cool", "file_size": 1200},
        {"title": "Also Wonderful", "file_size": 783}
    ]
    for row in data:
        r = db.insert("table_1", row)
        pprint(r)

    db.