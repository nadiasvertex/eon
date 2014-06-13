__author__ = 'Christopher Nelson'

import argparse
from functools import partial

import lmdb
import zmq
from zmq.eventloop import ioloop

from ds.engine import dispatch_command


def reply_handler(db, sock, events):
    msg = sock.recv_json()
    reply = dispatch_command(db, msg)
    sock.send_json(reply)


def main():
    parser = argparse.ArgumentParser(description='Process some integers.')

    parser.add_argument('--listen', dest='listen', action='store',
                        default="tcp://127.0.0.1:10001",
                        help='Listen on the requested port. default=%(default)s')

    parser.add_argument('--database', dest='db', action='store',
                        required=True,
                        help='Use the specified database file.')

    args = parser.parse_args()

    db = lmdb.open(args.db, lock=False, max_dbs=32768)

    loop = ioloop.IOLoop.instance()

    ctx = zmq.Context.instance()

    s = ctx.socket(zmq.REP)
    s.bind(args.listen)

    loop.add_handler(s, partial(reply_handler, db), zmq.POLLIN)
    loop.start()


if __name__ == "__main__":
    main()
