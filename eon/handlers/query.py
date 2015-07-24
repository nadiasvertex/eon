import asyncio
import json
import locale
import logging
from aiohttp import web
from eon import instance
from eon.error import code
from eon.error.util import get_error_message
from eon.handlers.common import response, error
from eon.query.parser import Parser

__author__ = 'Christopher Nelson'


@asyncio.coroutine
def query_handler(request):
    log = logging.getLogger(__name__)
    mi = request.match_info
    db_name = mi.get('db_name', "system")

    store = instance.get_store()
    db = store.get_database(db_name)
    language = locale.getdefaultlocale()[0]

    log.debug("querying database: %s", db_name)

    if request.content_length is None:
        return error(language, code.MISSING_REQUEST_BODY, {})

    query = yield from request.json()

    p = Parser(db, query)
    if not p.compile():
        return error(language, code.COMPILE_ERROR, {"message":p.get_message()})

    row_data = db.query(query, p.get_plan())

    body = {
        "success": True,
        "data": row_data
    }

    return response(body)