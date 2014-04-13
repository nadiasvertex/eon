"""
Distributes rows to the proper node.
"""
__author__ = 'Christopher Nelson'

import struct
from functools import reduce

from eon.engine.data.hash_ring import HashRing


class Info:
    def __init__(self, table_name, node):
        self.table_name = table_name
        self.node = node
        self.row_count = 0


class Distributor:
    """
    Data is distributed to nodes based on their data load for a given table. Whatever node has the lowest row count
    for that table will be handed the new row to insert. Nodes do not attempt to precisely synchronize inserts across
    the cluster, but they do perform a count synchronize periodically.
    """

    def __init__(self, nodes, models, sync_period=5):
        self.nodes = nodes
        self.hr = HashRing(nodes)
        self.info = [Info(model.table_name, node)
                     for model in models
                     for node in nodes]

    def get_nodes(self):
        return self.nodes

    def get_best_node(self, table_name, new_rows):
        """
        Finds the 'best' node to send a row to given the load. Note that this assumes that you are putting load on
        this node, so it adjusts it's internal counter accordingly. It isn't necessary to call this function for
        every single insert. It is better to call it once for a bulk load, or a multi-row insert. If the insert is
        truly enormous it would make sense to call this function every N rows.

        :param table_name: The name of the table where the row(s) should go.
        :param new_rows: The number of new rows to be added.
        :returns: The node where the row(s) should be sent.
        """
        info = reduce(lambda n1, n2: n1 if n1.row_count < n2.row_count else n2,
                      (info for info in self.info if info.table_name == table_name))
        info.row_count += new_rows
        return info.node

    def find_nodes_for_key(self, key):
        """
        Used when trying to find out where a row is stored. The return value is a generator which never ends. It
        will start with the most likely location and gradually fall back to other possible locations.

        :param key: A 64-bit integer that is the key for the data.
        """
        key_data = struct.pack("Q", key)
        return self.hr.get_nodes(key_data)

