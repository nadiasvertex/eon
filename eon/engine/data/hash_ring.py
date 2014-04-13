"""
A consistent hashing algorithm implementation adapted from code at http://amix.dk/blog/post/19367
"""
__author__ = 'Christopher Nelson'

import struct
from pyhashxx import hashxx


class HashRing(object):
    @staticmethod
    def _repackage_node(node):
        return node[0].encode("utf8"), int(node[1])

    @staticmethod
    def _get_key_format(node):
        return "%dsHB" % len(node[0])

    def __init__(self, nodes=None, replicas=3):
        """
        Manages a hash ring.

        :param nodes: is a list of tuples: (host,port) for each node
        :param replicas: indicates how many virtual points should be used per node,
        replicas are required to improve the distribution.
        """
        self.replicas = replicas

        self.ring = {}
        self._sorted_keys = []

        if nodes:
            for node in nodes:
                self.add_node(node)

    def add_node(self, node):
        """
        Adds a `node` to the hash ring (including a number of replicas).

        :param node: The node to add. A node is '(host, port)'.
        """

        original_node = node
        node = self._repackage_node(node)
        fmt = self._get_key_format(node)

        for i in range(0, self.replicas):
            node_key = struct.pack(fmt, node[0], node[1], i)
            key = hashxx(node_key)
            self.ring[key] = original_node
            self._sorted_keys.append(key)

        self._sorted_keys.sort()

    def remove_node(self, node):
        """
        Removes `node` from the hash ring and its replicas.

        :param node: The node to remove. A node is '(host, port)'.
        """

        node = self._repackage_node(node)
        fmt = self._get_key_format(node)

        for i in range(0, self.replicas):
            node_key = struct.pack(fmt, node[0], node[1], i)
            key = hashxx(node_key)
            del self.ring[key]
            self._sorted_keys.remove(key)

    def get_node(self, bytes_key):
        """
        Given a bytes object, a corresponding node in the hash ring is returned.

        If the hash ring is empty, `None` is returned.
        :param bytes_key: The key to use to lookup the node.
        """
        return self.get_node_pos(bytes_key)[0]

    def get_node_pos(self, bytes_key):
        """
        Given a bytes object, a corresponding node in the hash ring is returned
        along with it's position in the ring.

        If the hash ring is empty, (`None`, `None`) is returned.

        :param bytes_key: The key to use to lookup the position.
        """
        if not self.ring:
            return None, None

        key = hashxx(bytes_key)

        nodes = self._sorted_keys
        for i in range(0, len(nodes)):
            node = nodes[i]
            if key <= node:
                return self.ring[node], i

        return self.ring[nodes[0]], 0

    def get_nodes(self, bytes_key):
        """
        Given a bytes object, it returns the nodes that can hold the key as a generator.

        The generator is never ending and iterates through the ring
        starting at the correct position.

        :param bytes_key: The key to use to lookup the list of nodes that could hold the key.
        """
        if not self.ring:
            yield None, None

        node, pos = self.get_node_pos(bytes_key)
        for key in self._sorted_keys[pos:]:
            yield self.ring[key]

        while True:
            for key in self._sorted_keys:
                yield self.ring[key]
