from pprint import pprint
from eon.client.data import DataType

__author__ = 'Christopher Nelson'

from eon.client import cluster
from eon.client.exception import DataDefinitionLanguageError

with cluster.connect() as c:
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
