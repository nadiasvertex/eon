from pprint import pprint

__author__ = 'Christopher Nelson'

from eon.client import cluster
from eon.client.exception import DataDefinitionLanguageError

with cluster.connect() as c:
    try:
        db = c.open_database("test")
    except DataDefinitionLanguageError:
        db = c.create_database("test")

    pprint(db.schema)