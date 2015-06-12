import asyncio
import locale
import logging
from eon import instance
from eon.handlers.common import error, response
from eon.error import code

__author__ = 'Christopher Nelson'


@asyncio.coroutine
def handle_create_db(request):
    log = logging.getLogger(__name__)
    language = locale.getdefaultlocale()[0]

    mi = request.match_info
    db_name = mi.get('db_name')

    log.debug("database create: %s", db_name)

    store = instance.get_store()
    db = store.get_database(db_name)
    if db is not None:
        return error(language, code.DUPLICATE_SCHEMA_OBJECT, {"name": db_name})

    if request.content_length is not None:
        details = request.json()
        # For now we don't actually do anything
        # with the data that we read.

    db = store.create_database(db_name)
    store.store_site()

    body = {
        "success": True,
        "schema": db.store()
    }

    return response(body)


@asyncio.coroutine
def handle_get_db(request):
    log = logging.getLogger(__name__)
    language = locale.getdefaultlocale()[0]

    mi = request.match_info
    db_name = mi.get('db_name')

    log.debug("database get: %s", db_name)

    store = instance.get_store()
    db = store.get_database(db_name)
    if db is None:
        return error(language, code.UNKNOWN_SCHEMA_OBJECT, {"name": db_name})

    body = {
        "success": True,
        "schema": db.store()
    }

    return response(body)

@asyncio.coroutine
def handle_get_db_list(request):
    log = logging.getLogger(__name__)

    log.debug("cluster get: list of databases")

    store = instance.get_store()

    body = {
        "success": True,
        "schema": list(store.get_databases())
    }

    return response(body)
