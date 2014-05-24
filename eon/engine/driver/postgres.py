__author__ = 'Christopher Nelson'

import psycopg2


class ConnectionContextManager:
    def __init__(self, node, pool):
        self.node = node
        self.pool = pool
        self.conn = None

    def __enter__(self):
        self.conn = self.pool.acquire_connection(self.node)
        return self.conn

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.pool.release_connection(self.node, self.conn)
        return False


class ConnectionPool:
    def __init__(self, database_name, user_name, password):
        self.database_name = database_name
        self.user_name = user_name
        self.password = password
        self.connections = {}

    def acquire_connection(self, node):
        l = self.connections.get(node, [])
        if not l:
            return psycopg2.connect(
                database=self.database_name,
                user=self.user_name,
                password=self.password,
                host=node[0],
                port=node[1]
            )

        return l.pop()

    def release_connection(self, node, conn):
        l = self.connections.setdefault(node, [])
        l.append(conn)

    def connect(self, node):
        return ConnectionContextManager(node, self)


class Driver:
    def __init__(self):
        pass

    def load(self, model, selected_fields=None):
        """
        This function loads
        """
        pass