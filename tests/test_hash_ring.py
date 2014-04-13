import unittest

from eon.engine.data.hash_ring import HashRing


class TestHashRing(unittest.TestCase):
    nodes = [
            ('a', 100),
            ('b', 100),
            ('c', 100)
    ]

    nodes_large = [
            ('a', 100),
            ('b', 100),
            ('c', 100),
            ('d', 100),
            ('e', 100),
            ('f', 100)
    ]

    def test_can_add_nodes(self):
        hr = HashRing(self.nodes)

    def test_can_remove_node(self):
        hr = HashRing(self.nodes)
        hr.remove_node(self.nodes[1])

    def test_hashing_works(self):
        buckets = {n:{} for n in self.nodes}

        hr = HashRing(self.nodes)
        data = {("key%d" % i).encode("utf8"):i for i in range(0, 1000)}

        # Distribute the data through the nodes
        for k, v in data.items():
            node = hr.get_node(k)
            buckets[node][k]=v

        # Now see if we can get it back.
        for k, v in data.items():
            node = hr.get_node(k)
            self.assertEqual(buckets[node][k], v)

    def test_consistent_hashing_works(self):
        buckets = {n: {} for n in self.nodes_large}

        hr = HashRing(self.nodes_large[0:3])
        data = {("key%d" % i).encode("utf8"):i for i in range(0, 10000)}

        # Distribute the data through the nodes
        i=0; next_node=3
        for k, v in data.items():
            node = hr.get_node(k)
            buckets[node][k]=v
            i+=1
            if next_node<len(self.nodes_large) and i%1000==0:
                hr.add_node(self.nodes_large[next_node])
                next_node+=1

        # Now see if we can get it back.
        for k, v in data.items():
            for node in hr.get_nodes(k):
                if k in buckets[node]:
                    self.assertEqual(buckets[node][k], v)
                    break
            else:
                self.fail("Cannot find key '%s'" % k)
