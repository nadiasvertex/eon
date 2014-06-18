__author__ = 'Christopher Nelson'

import queue

import msgpack
import zmq

from ds.hash_ring import HashRing
from zmq.eventloop import ioloop


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

    def request_row_from_indexed_column(self, key, request):
        """
        In cases where we know that some node has an index with 'key'
        in it, we determine which node that is. Then we ask that node
        to look up that key and fetch the row for us.

        :param key:
        :param request:
        :return:
        """







