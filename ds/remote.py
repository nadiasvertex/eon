__author__ = 'Christopher Nelson'

import io
import queue
import struct

import msgpack
import zmq
from zmq.eventloop import ioloop

from ds.hash_ring import HashRing


REQ_FETCH_INDEXED = 0
REQ_FETCH_SCAN = 1

REP_NOT_FOUND = 0
REP_FOUND = 1
REP_UNKNOWN_COMMAND = 0x0f


class JoinManager:
    def __init__(self, peers):
        # Get our ZMQ context.
        loop = ioloop.IOLoop.instance()
        ctx = zmq.Context.instance()

        # Create request sockets for each peer and bind them.
        self.sockets = {peer: ctx.socket(zmq.REQ) for peer in peers}
        # Create a response queue for all peers.
        self.queue = queue.Queue()
        # Create a hash ring for determining which peer is most likely
        # to have the information desired.
        self.ring = HashRing(peers)

        # Connect the sockets to the corresponding pears.
        for address, s in self.sockets.items():
            s.connect(address)
            loop.add_handler(s, self._response_handler, zmq.POLLIN)

    def _response_handler(self, s, events):
        self.queue.put(msgpack.unpackb(s.recv()))

    def get_response(self):
        try:
            return self.queue.get(block=False)
        except queue.Empty:
            return None

    def fetch_row_indexed(self, key, request):
        """
        In cases where we know that some node has an index with 'key'
        in it, we determine which node that is. Then we ask that node
        to look up that key and fetch the row for us.

        :param key: The value to look for.
        :param request: The request specifying all the information needed
        by the remote to fetch the row.
        """
        # Figure out which node owns this row.
        owner = self.ring.get_node(key)

        # Get the socket connected to this peer.
        s = self.sockets.get(owner)

        # Send the request.
        s.send(msgpack.packb((REQ_FETCH_INDEXED, request)))

    def fetch_row_scan(self, request):
        """
        In cases where we don't have an index, we have to do a
        broadcast lookup. All nodes are sent a fetch request,
        they all do a table scan, and

        :param request: The request specifying all the information needed
        by the remote to fetch the row.
        """
        for s in self.sockets.values():
            s.send(msgpack.packb((REQ_FETCH_SCAN, request)))


def cmd_fetch_scan(db, cmd, request):
    """
    Perform a scan of the table and find all the rows that match a given value.

    :param db:
    :param cmd:
    :param request:
    :return:
    """
    remote_row_id, target_column_value, schema_info = request
    _, column_idx, table_name = schema_info

    rows = []

    # Scan the table looking for rows with the value specified.
    table = db.open_db(table_name.encode("utf8"), create=True)  # Read the data
    with db.begin(db=table, buffers=True) as txn:
        cursor = txn.cursor()
        for item in iter(cursor):
            # Use a streaming unpacker to avoid having to copy
            # bytes from the database.
            u = msgpack.Unpacker(io.BytesIO(cursor.value()))
            present, value = u.unpack()

            if column_idx not in present:
                continue

            idx = present.index(column_idx)
            column_value = value[idx]
            if target_column_value == column_value:
                rows.append(struct.unpack_from("Q", cursor.key()))

    return (REP_FOUND, (remote_row_id, rows)) if rows else (REP_NOT_FOUND, remote_row_id)


def dispatch_command(db, msg):
    """
    Here we handle the very simple 'find row with this value in this column' commands from other engines.

    :param db: The database to use.
    :param msg: The message to process
    :return: A response to the remote process.
    """
    cmd, request = msg
    if cmd == REQ_FETCH_SCAN:
        return cmd_fetch_scan(db, cmd, request)

    return [REP_UNKNOWN_COMMAND]


def reply_handler(db, sock, events):
    msg = msgpack.unpackb(sock.recv())
    reply = dispatch_command(db, msg)
    sock.send(msgpack.packb(reply))


def start(db, ctx, loop, listen_address):
    """
    This function starts the remote engine response processor. This processor is used by the system to process
    engine to engine traffic for things like distributed joins and distributed index management.

    :param ctx: The ZMQ context.
    :param loop: The run loop.
    :param listen_address: The address the engine response processor should listen on.
    :return: None
    """
    s = ctx.socket(zmq.REP)
    s.bind(listen_address)

    loop.add_handler(s, partial(reply_handler, db), zmq.POLLIN)
