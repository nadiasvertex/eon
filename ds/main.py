__author__ = 'Christopher Nelson'

import argparse
import json
import struct
from datetime import datetime
from enum import Enum
from functools import partial

import lmdb

import msgpack
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


def get_seq_key(table_name):
    return (".seq.%s" % table_name).encode("utf-8")


def get_column_presence(all, present):
    present_indexes = []
    for pn in present:
        for i, an in enumerate(all):
            if pn != an:
                continue
            present.append(i)
            break
    return present_indexes


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

    with db.open_db(".metadatabase", create=True) as meta_db:
        # See if it already exists.
        with db.begin(db=meta_db) as txn:
            if txn.get(table_key) is not None:
                return {"error": "Unable to create table '%s' because it already exists." % table_name}

        with db.begin(db=meta_db, write=True) as txn:
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

    with db.open_db(".metadatabase", create=True) as meta_db:
        # See if it already exists.
        with db.begin(db=meta_db) as txn:
            schema = txn.get(table_key)
            if schema is None:
                return {"error": "Unable to delete table '%s' because it doesn't exist." % table_name}

        schema = json.load(schema.decode("utf-8"))
        with db.begin(db=meta_db, write=True) as txn:
            for name in schema["columns"]:
                column_key = get_schema_key(Schema.column, table_name, name)
                txn.put(column_key, name.encode("utf-8"))

            txn.commit()


def cmd_put(db, cmd):
    """
    We expect the command form for put to be:
    {
     "cmd"     : 3,
     "name"    : "some_table_name"
     "columns" : ["col1", "col2"]
     "values"  : [[1, "v2"]]
    }

    :param db: The database to read/write to.
    :param cmd: The command to process.
    :return: An indication of success or failure.
    """
    table_name = cmd["name"]
    table_key = get_schema_key(Schema.table, "", table_name)

    with db.open_db(".metadatabase") as meta_db:
        # See if it already exists.
        with db.begin(db=meta_db) as txn:
            schema = txn.get(table_key)
            if schema is None:
                return {"error": "Unable to write to table '%s' because it doesn't exist." % table_name}

    # See if we referenced columns that don't exist.
    present_columns = cmd["columns"]
    all_columns = schema["columns"]
    unknown_columns = set(present_columns) - set(all_columns)
    if unknown_columns != 0:
        return {"error": "Unable to write to table '%s' because column(s) %s do not exist" % (
            table_name, ",".join(unknown_columns))}

    present_indexes = get_column_presence(all_columns, present_columns)
    with db.open_db(table_name, create=True) as table:
        # Write the data
        st = db.stat(table)
        next_id = st["entries"]
        with db.begin(db=table, write=True, buffers=True) as txn:
            for value in cmd["values"]:
                txn.put(struct.pack("Q", next_id),
                        msgpack.packb([present_indexes, value]),
                        append=True)
                next_id += 1
            txn.commit()


def cmd_get(db, cmd):
    pass


def cmd_update(db, cmd):
    pass


dispatch_table = {
    1: cmd_create_table,
    2: cmd_drop_table,
    3: cmd_put,
    4: cmd_get,
    5: cmd_update
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

    db = lmdb.open(args.db, lock=False, max_dbs=32768)

    loop = ioloop.IOLoop.instance()

    ctx = zmq.Context.instance()

    s = ctx.socket(zmq.REP)
    s.bind(args.listen)

    loop.add_handler(s, partial(reply_handler, db), zmq.POLLIN)
    loop.start()


if __name__ == "__main__":
    main()
