import argparse
import asyncio
import json
import locale
import logging
import os
from urllib.parse import urlsplit

from aiohttp import web

from eon.error import code
from eon.error.util import get_error_message
from eon.schema.store import Store

__author__ = 'Christopher Nelson'


@asyncio.coroutine
def handle_restful_table_get(request):
    mi = request.match_info
    db_name = mi.get('db_name', "system")
    table_name = mi.get('table_name')
    rid = mi.get("row_id")
    response = {
        "success": False
    }
    return web.Response(body=json.dumps(response).encode('utf-8'))


@asyncio.coroutine
def query_handler(request):
    mi = request.match_info
    db_name = mi.get('db_name', "system")
    db = instance_store.get_database(db_name)
    language = locale.getdefaultlocale()[0]

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


@asyncio.coroutine
def init(loop, address, port):
    log = logging.getLogger()
    app = web.Application(loop=loop)
    app.router.add_route('GET', '/q/{db_name}', query_handler)
    app.router.add_route('GET', '/{db_name}/{table_name}/{row_id}', handle_restful_table_get)

    srv = yield from loop.create_server(app.make_handler(), address, port)
    log.info("Server started at http://%s:%s" % (address, port))
    return srv


def main():
    global instance_store
    parser = argparse.ArgumentParser(prog="eon", description="eon database storage backend")

    parser.add_argument(
        "--service", default="http://localhost:8080/",
        help="The address at which to provide service."
    )

    parser.add_argument(
        "--data", default=os.path.normpath(os.path.expanduser("~/.eon-data")),
        help="The base folder for data. Each service will create a sub-folder under this path to store its data. "
             "default=%(default)s"
    )

    args = parser.parse_args()
    svc = urlsplit(args.service)
    if ":" in svc.netloc:
        address, port = svc.netloc.split(":")
    else:
        address = svc.netloc
    port = "8080"

    base_data_dir = os.path.join(args.data, svc, port)
    instance_store = Store(base_data_dir)

    loop = asyncio.get_event_loop()
    loop.run_until_complete(init(loop, address, port))
    loop.run_forever()


if __name__ == "__main__":
    instance_store = None
    main()
