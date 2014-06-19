__author__ = 'Christopher Nelson'

import queue

import msgpack
import zmq
from zmq.eventloop import ioloop

from ds.hash_ring import HashRing

FETCH_INDEXED = 0
FETCH_SCAN = 1


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
        s.send(msgpack.packb(request))

    def fetch_row_scan(self, request):
        """
        In cases where we don't have an index, we have to do a
        broadcast lookup. All nodes are sent a fetch request,
        they all do a table scan, and

        :param request: The request specifying all the information needed
        by the remote to fetch the row.
        """
        for s in self.sockets.values():
            s.send(msgpack.packb(request))
