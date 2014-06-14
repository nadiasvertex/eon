__author__ = 'Christopher Nelson'

# system modules
import bisect
import io
import struct
from datetime import datetime
from enum import Enum

# support modules
import msgpack

# eon modules
from ds import compute

META_DATABASE = ".meta_database".encode("utf-8")


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


def get_column_index(all_columns, column):
    """
    Locate the column in the sorted list of all_columns.
    """
    i = bisect.bisect_left(all_columns, column)
    if i != len(all_columns) and all_columns[i] == column:
        return i
    return None


def get_column_presence(all_columns, present_columns):
    present_indexes = []
    for pn in present_columns:
        idx = get_column_index(all_columns, pn)
        if idx is not None:
            present_indexes.append(idx)
    return present_indexes


def check_table_presence(db, table_key):
    meta_db = db.open_db(META_DATABASE, create=True)
    # See if it already exists.
    with db.begin(db=meta_db) as txn:
        schema = txn.get(table_key)

    return None if schema is None else msgpack.unpackb(schema)


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

    meta_db = db.open_db(META_DATABASE, create=True)

    # See if it already exists.
    with db.begin(db=meta_db) as txn:
        if txn.get(table_key) is not None:
            return {"status": False, "error": "Unable to create table '%s' because it already exists." % table_name}

    with db.begin(db=meta_db, write=True) as txn:
        info = {"created": datetime.now().isoformat(), "columns": sorted([c["name"] for c in cmd["columns"]])}
        packed = msgpack.packb(info)
        txn.put(table_key, packed)

        for column in cmd["columns"]:
            column_key = get_schema_key(Schema.column, table_name, column["name"])
            txn.put(column_key, msgpack.packb(column))

    return {"status": True}


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

    meta_db = db.open_db(META_DATABASE, create=True)
    # See if it already exists.
    with db.begin(db=meta_db) as txn:
        schema = txn.get(table_key)
        if schema is None:
            return {"status": False, "error": "Unable to delete table '%s' because it doesn't exist." % table_name}

    schema = msgpack.unpackb(schema)
    with db.begin(db=meta_db, write=True) as txn:
        for name in schema[b'columns']:
            column_key = get_schema_key(Schema.column, table_name, name)
            txn.delete(column_key)

    return {"status": True}


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

    schema = check_table_presence(db, table_key)
    if schema is None:
        return {"status": False,
                "error": "Unable to write to table '%s' because it doesn't exist." % table_name}

    # See if we referenced columns that don't exist.
    present_columns = [name.encode("utf8") for name in cmd["columns"]]
    all_columns = schema[b'columns']
    unknown_columns = set(present_columns) - set(all_columns)
    if len(unknown_columns) != 0:
        return {"status": False, "error": "Unable to write to table '%s' because column(s) %s do not exist" % (
            table_name, ",".join(unknown_columns))}

    present_indexes = get_column_presence(all_columns, present_columns)
    table = db.open_db(table_name.encode("utf8"), create=True)  # Write the data
    with db.begin(db=table, write=True, buffers=True) as txn:
        st = txn.stat(table)
        next_id = st["entries"]
        for value in cmd["values"]:
            txn.put(struct.pack("Q", next_id),
                    msgpack.packb([present_indexes, value]),
                    append=True)
            next_id += 1

    return {"status": True}


def cmd_get(db, cmd):
    """
    We expect the command form for get to be:
    {
     "cmd"       : 4,
     "name"      : "some_table_name"
     "predicates":[
        {"op":"lt", "args":[[0, "test_col_1], [1, 5]},
        ["op":"eq", "args":[[0,"test_col_2"], [1, "dog"]]}
     ]
    }

    :param db: The database to read/write to.
    :param cmd: The command to process.
    :return: An indication of success or failure.
    """
    table_name = cmd["name"]
    table_key = get_schema_key(Schema.table, "", table_name)

    schema = check_table_presence(db, table_key)
    if schema is None:
        return {"status": False,
                "error": "Unable to read from table '%s' because it doesn't exist." % table_name}

    columns = schema[b'columns']
    predicates = cmd["predicates"]

    # Process the column names into integers, also perform error
    # checking.
    for p in predicates:
        for arg in p["args"]:
            # If argument is a column
            if arg[0] == 0:
                idx = get_column_index(columns, arg[1].encode("utf8"))
                if idx is None:
                    return {"status": False,
                            "error": "Unable to read from table '%s' because column '%s' doesn't exist." % (
                            table_name, arg[1])}
                arg[1] = idx

    rows = []

    # We don't have indexes, fall back to a table scan.
    table = db.open_db(table_name.encode("utf8"), create=True)  # Write the data
    with db.begin(db=table, buffers=True) as txn:
        cursor = txn.cursor()
        for item in iter(cursor):
            row_id = struct.unpack_from("Q", cursor.key())

            # Use a streaming unpacker to avoid having to copy
            # bytes from the database.
            u = msgpack.Unpacker(io.BytesIO(cursor.value()))
            data = u.unpack()

            # Get the present index for this row, and the stored
            # data.
            present = data[0]
            value = data[1]

            # Process each predicate
            for p in predicates:
                op = getattr(compute, p["op"])
                args = p["args"]
                processed_args = [compute.get_value(present, value, arg)
                                  for arg in args]
                if not op(*processed_args):
                    break
            else:
                rows.append(row_id)

    return {"status": True, "data": rows}


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

