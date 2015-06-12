import asyncio
import locale
import logging
from eon import instance
from eon.handlers.common import error, response
from eon.error import code

__author__ = 'Christopher Nelson'


@asyncio.coroutine
def handle_create_table(request):
    log = logging.getLogger(__name__)
    language = locale.getdefaultlocale()[0]

    mi = request.match_info
    db_name = mi.get('db_name')
    table_name = mi.get('table_name')

    log.debug("table create: %s.%s", db_name, table_name)

    if request.content_length is None:
        return error(language, code.MISSING_REQUEST_BODY, {})

    details = yield from request.json()

    store = instance.get_store()
    worked, error_code, parms = store.create_table(db_name, table_name, details)
    if not worked:
        return error(language, error_code, parms)

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

@asyncio.coroutine
def handle_raw_table_put(request):
    log = logging.getLogger(__name__)
    language = locale.getdefaultlocale()[0]

    mi = request.match_info
    db_name = mi.get('db_name')
    table_name = mi.get('table_name')

    log.debug("insert: %s.%s", db_name, table_name)

    if request.content_length is None:
        return error(language, code.MISSING_REQUEST_BODY, {})

    details = yield from request.json()
    if not details:
        return error(language, code.MISSING_REQUEST_BODY, {})

    log.debug("insert: %s", details)

    store = instance.get_store()
    worked, error_code_or_rid, params = store.insert(db_name, table_name, details)

    if not worked:
        return error(language, error_code_or_rid, params)

    body = {
        "success": True,
        "data": error_code_or_rid
    }

    return response(body)
