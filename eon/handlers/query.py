import asyncio
import json
import locale
import logging
from aiohttp import web
from eon import instance
from eon.error import code
from eon.error.util import get_error_message

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

    ws = web.WebSocketResponse()
    ws.start(request)

    while True:
        msg = yield from ws.receive()
        if msg.tp == web.MsgType.text:
            data = json.loads(msg.data)
            if db is None:
                ws.send_str(json.dumps({
                    "success": False,
                    "message": get_error_message(language, code.UNKNOWN_SCHEMA_OBJECT)
                }))
            else:
                ws.send_str(json.dumps({
                    "success": True,
                    "data": db.query(data)
                }))
        elif msg.tp == web.MsgType.close:
            break

    return ws