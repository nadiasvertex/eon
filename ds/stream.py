__author__ = 'Christopher Nelson'

import io

import msgpack
import zmq
from zmq.eventloop import ioloop

from ds import compute

from ds.hash_ring import HashRing


def process(self, db, table, raw_predicates):
    """
    The predicates should be pre-parsed so that they have the column index numbers and not the column names.

    The raw_predicates structure should look like this:
    [
     {op:"lt", args:[[0,0], [1,100]]},
     {op:"eq", args:[[0,1], [1,"test"]]}
    ]

    :param db: The master database.
    :param table: The child database that contains the table.
    :param raw_predicates: The predicates.
    :return: The row_id for each matching row. Note that this is in packed format.

    :note: A read transaction will be opened for the duration of this method.
    """

    # First, process the predicates so that we get the function to run.
    predicates = [(getattr(compute, p["op"]), p["args"]) for p in raw_predicates]

    # Next, open the table and provide a yield for each matching row.
    with db.begin(db=table, buffers=True) as txn:
        cursor = txn.cursor()
        for item in iter(cursor):
            # Use a streaming unpacker to avoid having to copy
            # bytes from the database.
            u = msgpack.Unpacker(io.BytesIO(cursor.value()))
            present, value = u.unpack()

            # Process each simple predicate in turn.
            for p in predicates:
                op, args = p
                processed_args = [compute.get_value(self.present, self.value, arg)
                                  for arg in args]
                if not op(*processed_args):
                    break
            else:
                yield cursor.key()


def inner_join_scan(self, source, db, local_table, local_column_idx, joined_column_reference, peers):
    """
    Performs an inner join by reading a row_id from the source generator, then attempting to join
    that column by scanning the entire distributed table space for rows that match the joined_column_reference.

    :param source: A source generator that provides row ids to look for.
    :param db: The master database.
    :param local_table: The child database that contains the local table to lookup the row ids in.
    :param local_column_idx: The local column that has the value we are trying to join on.
    :param joined_column_reference: The table.column(op) reference which tells us what we are joining on.
    :return: The row_id for each matching row. Note that this is in packed format.
    """

    # Get our ZMQ context.
    loop = ioloop.IOLoop.instance()
    ctx = zmq.Context.instance()
    poller = zmq.Poller()

    # Create request sockets for each peer and bind them.
    sockets = {peer: ctx.socket(zmq.REQ) for peer in peers}

    # Connect the sockets to the corresponding pears.
    for address, s in self.sockets.items():
        s.connect(address)
        poller.register(s, zmq.POLLIN)

    # Next, open the table and provide a yield for each matching row.
    with db.begin(db=local_table, buffers=True) as txn:
        for row_id in source:
            present, values = msgpack.unpackb(txn.get(row_id))
            column_value = compute.get_column_value(present, values, local_column_idx)

            # Now


