import argparse
import asyncio
import logging
import os
from urllib.parse import urlsplit

from aiohttp import web
from eon import instance

from eon.handlers.db import handle_create_db, handle_get_db
from eon.handlers.query import query_handler
from eon.handlers.table import handle_create_table, handle_raw_table_get

__author__ = 'Christopher Nelson'


@asyncio.coroutine
def init(loop, address, port):
    log = logging.getLogger()
    app = web.Application(loop=loop)
    app.router.add_route('GET', '/q/{db_name}', query_handler)

    # Raw table access
    app.router.add_route('GET', '/r/{db_name}/{table_name}/{row_id}', handle_raw_table_get)

    # Schema access
    app.router.add_route('PUT', '/s/{db_name}/{table_name}', handle_create_table)
    app.router.add_route('PUT', '/s/{db_name}', handle_create_db)
    app.router.add_route('GET', '/s/{db_name}', handle_get_db)

    srv = yield from loop.create_server(app.make_handler(), address, port)
    log.info("Server started at http://%s:%s" % (address, port))
    return srv


def main():
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
    instance.create_store(base_data_dir)

    loop = asyncio.get_event_loop()
    loop.run_until_complete(init(loop, address, port))
    loop.run_forever()


if __name__ == "__main__":
    main()
