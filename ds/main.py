__author__ = 'Christopher Nelson'

import argparse
import json
from datetime import datetime
from enum import Enum
from functools import partial

import lmdb

import zmq
from zmq.eventloop import ioloop


class Schema(Enum):
    table = 1
    column = 2


def get_schema_key(schema_type, parent_name, name):
    if schema_type == Schema.table:
        s_type = "table"
    elif schema_type == Schema.column:
        s_type = "column"

    return (".object.%s.%s.%s" % (s_type, parent_name, name)).encode("utf-8")


def cmd_create_table(db, cmd):
    """
    We expect the command form for create table to be:
    {
     "cmd"      : 1,
     "name"    : "some_table_name",
     "columns" : [{"name" : "mycol", "type" : "int"}, {"name" : "mycol2", "type" : "text"}]
    }

    :param db: The database to read/write to.
    :param cmd: The command to process.
    :return: An indication of success or failure.
    """
    table_name = cmd["name"]
    table_key = get_schema_key(Schema.table, "", table_name)
    # See if it already exists.
    with db.begin() as txn:
        if txn.get(table_key) is not None:
            return {"error": "Unable to create table '%s' because it already exists." % table_name}

    with db.begin(write=True) as txn:
        info = {"created": datetime.now().isoformat(), "columns": [c["name"] for c in cmd["columns"]]}
        txn.put(table_key, json.dump(info).encode("utf-8"))

        for column in cmd["columns"]:
            column_key = get_schema_key(Schema.column, table_name, column["name"])
            txn.put(column_key, column.encode("utf-8"))

        txn.commit()


def cmd_drop_table(db, cmd):
    """
    We expect the command form for drop table to be:
    {
     "cmd"      : 2,
     "name"    : "some_table_name"
    }

    :param db: The database to read/write to.
    :param cmd: The command to process.
    :return: An indication of success or failure.
    """
    table_name = cmd["name"]
    table_key = get_schema_key(Schema.table, "", table_name)
    # See if it already exists.
    with db.begin() as txn:
        schema = txn.get(table_key)
        if schema is None:
            return {"error": "Unable to delete table '%s' because it doesn't exist." % table_name}

    


dispatch_table = {
    1: cmd_create_table,
    2: cmd_drop_table
}


def dispatch_command(db, cmd):
    cmd_processor = dispatch_table.get(cmd["cmd"])
    if cmd_processor:
        return cmd_processor(db, cmd)
    return {"error": "Unknown command."}


def reply_handler(db, sock, events):
    msg = sock.recv_json()
    reply = dispatch_command(msg)
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

    db = lmdb.open(args.dest)

    loop = ioloop.IOLoop.instance()

    ctx = zmq.Context.instance()

    s = ctx.socket(zmq.REP)
    s.bind(args.listen)

    loop.add_handler(s, partial(reply_handler, db), zmq.POLLIN)
    loop.start()


if __name__ == "__main__":
    main()




