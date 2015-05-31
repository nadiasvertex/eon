import asyncio
import json
import locale
import logging
from aiohttp import web
from eon import instance
from eon.handlers.common import error, response
from eon.error import code
from eon.schema.data import DataType
from eon.schema.table import Table

__author__ = 'Christopher Nelson'

data_type_values = [i.value for i in DataType]
required_column_ddl_fields = ("name", "data-type")
default_column_ddl_values = {"nullable": True, "default": None, "max-length": None}
scalar_value_types = (DataType.big_int, DataType.standard_int, DataType.small_int, DataType.bool)

@asyncio.coroutine
def handle_create_table(request):
    log = logging.getLogger(__name__)
    language = locale.getdefaultlocale()[0]

    mi = request.match_info
    db_name = mi.get('db_name')
    table_name = mi.get('table_name')

    log.debug("table create: %s.%s", db_name, table_name)

    store = instance.get_store()
    db = store.get_database(db_name)
    if db is None:
        return error(language, code.UNKNOWN_SCHEMA_OBJECT, {"name": db_name})

    table = db.get_table(table_name)
    if table is not None:
        return error(language, code.DUPLICATE_SCHEMA_OBJECT, {"name": ".".join([db_name, table_name])})

    if request.content_length is None:
        return error(language, code.MISSING_REQUEST_BODY, {})

    details = request.json()
    columns = details.get("columns")
    if columns is None:
        return error(language, code.MALFORMED_SCHEMA_SPEC, {"required_field_name": "columns"})

    new_columns = []
    for i, column in enumerate(columns):
        # Check for required fields
        for field in required_column_ddl_fields:
            if field not in column:
                return error(language, code.MALFORMED_SCHEMA_SPEC, {
                    "required_field_name": "columns[%d].%s" % (i, field)
                })

        # Make sure the field values are within required limits.
        if column["data-type"] not in data_type_values:
            return error(language, code.UNKNOWN_DATA_TYPE, {
                "value": column["data-type"],
                "allowed_values": {i.name: i.value for i in DataType}
            })

        if "max-length" in column:
            dt = DataType(column["data-type"])
            max_length = column["max-length"]
            if max_length is not None:
                if max_length <= 0 or dt in scalar_value_types:
                    return error(language, code.ILLEGAL_DATA_LENGTH, {
                        "name": dt,
                        "value": max_length
                    })

        # Fill in default values
        new_column = default_column_ddl_values.copy()
        new_column.update(column)
        new_columns.append(new_column)

    # Create the new table
    table = Table(name=table_name, columns=new_columns)
    db.create_table(table)

    # Save new schema
    store.store_site()

    body = {
        "success": True
    }

    return response(body)


@asyncio.coroutine
def handle_raw_table_get(request):
    log = logging.getLogger(__name__)
    language = locale.getdefaultlocale()[0]

    mi = request.match_info
    db_name = mi.get('db_name', "system")
    table_name = mi.get('table_name')
    rid = mi.get("row_id")

    log.debug("table get: %s/%s/%s", db_name, table_name, rid)

    store = instance.get_store()
    db = store.get_database(db_name)
    if db is None:
        return error(language, code.UNKNOWN_SCHEMA_OBJECT, {"name": db_name})

    table = db.get_table(table_name)
    if table is None:
        return error(language, code.UNKNOWN_SCHEMA_OBJECT, {"name": db_name})

    try:
        row_data = table[int(rid)]
    except IndexError:
        return error(language, code.INDEX_ERROR, {"rid": rid, "object_name": ".".join([db_name, table_name])})

    body = {
        "success": True,
        "data": row_data
    }

    return response(body)
