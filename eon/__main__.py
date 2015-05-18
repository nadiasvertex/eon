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


def error(language, error_code, args):
    return web.Response(body=json.dumps({
        "success": False,
        "error_code": error_code,
        "message": get_error_message(language, error_code).format(**args)
    }).encode("utf-8"))


@asyncio.coroutine
def handle_raw_table_get(request):
    log = logging.getLogger(__name__)
    language = locale.getdefaultlocale()[0]

    mi = request.match_info
    db_name = mi.get('db_name', "system")
    table_name = mi.get('table_name')
    rid = mi.get("row_id")

    log.debug("table get: %s/%s/%s", db_name, table_name, rid)

    db = instance_store.get_database(db_name)
    if db is None:
        return error(language, code.UNKNOWN_SCHEMA_OBJECT, {"name": db_name})

    table = db.get_table(table_name)
    if table is None:
        return error(language, code.UNKNOWN_SCHEMA_OBJECT, {"name": db_name})

    try:
        row_data = table[int(rid)]
    except IndexError:
        return error(language, code.INDEX_ERROR, {"rid": rid, "object_name": ".".join([db_name, table_name])})

    response = {
        "success": True,
        "data": row_data
    }

    return web.Response(body=json.dumps(response).encode('utf-8'))

@asyncio.coroutine
def handle_create_db(request):
    log = logging.getLogger(__name__)
    language = locale.getdefaultlocale()[0]

    mi = request.match_info
    db_name = mi.get('db_name')

    log.debug("database create: %s", db_name)

    db = instance_store.get_database(db_name)
    if db is None:
        return error(language, code.UNKNOWN_SCHEMA_OBJECT, {"name": db_name})

    instance_store.create_database(db_name)

    if request.content_length is not None:
        details = request.json()
        # For now we don't actually do anything
        # with the data that we read.

    response = {
        "success": True
    }

    return web.Response(body=json.dumps(response).encode('utf-8'))

@asyncio.coroutine
def handle_create_table(request):
    log = logging.getLogger(__name__)
    language = locale.getdefaultlocale()[0]

    mi = request.match_info
    db_name = mi.get('db_name')
    table_name = mi.get('table_name')

    log.debug("table create: %s.%s", db_name, table_name)

    db = instance_store.get_database(db_name)
    if db is None:
        return error(language, code.UNKNOWN_SCHEMA_OBJECT, {"name": db_name})

    table = db.get_table(db_name)
    if table is not None:
        return error(language, code.DUPLICATE_SCHEMA_OBJECT, {"name": ".".join([db_name, table_name])})

    db.

    if request.content_length is not None:
        details = request.json()
        # For now we don't actually do anything
        # with the data that we read.

    response = {
        "success": True
    }

    return web.Response(body=json.dumps(response).encode('utf-8'))


@asyncio.coroutine
def query_handler(request):
    log = logging.getLogger(__name__)
    mi = request.match_info
    db_name = mi.get('db_name', "system")
    db = instance_store.get_database(db_name)
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


@asyncio.coroutine
def init(loop, address, port):
    log = logging.getLogger()
    app = web.Application(loop=loop)
    app.router.add_route('GET', '/q/{db_name}', query_handler)

    # Raw table access
    app.router.add_route('GET', '/r/{db_name}/{table_name}/{row_id}', handle_raw_table_get)

    # Schema access
    app.router.add_route('PUT', '/s/{db_name}', handle_create_db)

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

    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(message)s")

    svc = urlsplit(args.service)
    if ":" in svc.netloc:
        address, port = svc.netloc.split(":")
    else:
        address = svc.netloc
    port = "8080"

    base_data_dir = os.path.join(args.data, address, port)
    instance_store = Store(base_data_dir)

    loop = asyncio.get_event_loop()
    loop.run_until_complete(init(loop, address, port))
    loop.run_forever()


if __name__ == "__main__":
    instance_store = None
    main()
