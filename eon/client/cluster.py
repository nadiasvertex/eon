import asyncio
import json
import aiohttp
from eon.client import db
from eon.client.exception import DataDefinitionLanguageError

__author__ = 'Christopher Nelson'


class Connection:
    def __init__(self, protocol="http", host="localhost", port=8080):
        self.protocol = protocol
        self.host = host
        self.port = port
        self.base_url = "%s://%s:%s/" % (protocol, host, port)
        self.session = aiohttp.ClientSession()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.session.close()

    @asyncio.coroutine
    def _cmd(self, path: str, method: str, payload):
        """
        Issues a command by writing payload to path (if payload is not None) or just issuing a request to the given
        path. The payload is expected to be convertible to JSON. The result will be standard Python objects parsed
        out of the response.

        This is the worker method which performs the task asynchronously

        :param path: The URL path to issue the command to.
        :param payload: The payload, if any.
        :param method: The HTTP method to use.
        :return: An asynchronous generator.
        """
        response = yield from self.session.request(
            method, self.base_url + path,
            headers={'content-type': 'application/json'},
            data=None if payload is None else json.dumps(payload)
        )
        return (yield from response.json())

    def cmd(self, path: str, method="GET", payload=None, sync=True):
        """
        Issues a command by writing payload to path (if payload is not None) or just issuing a request to the given
        path. The payload is expected to be convertible to JSON. The result will be standard Python objects parsed
        out of the response.

        :param path: The URL path to issue the command to.
        :param payload: The payload, if any.
        :param method: The HTTP method to use.
        :param sync: If True, the command will be executed synchronously. If you pass false to this function
                     it becomes your responsibility to resolve the execution conditions for this command.
        :return: An asynchronous generator if sync is False, or a python object with the result.
        """
        r = self._cmd(path, method, payload)
        if not sync:
            return r

        f = asyncio.Task(r)
        loop = asyncio.get_event_loop()
        loop.run_until_complete(f)

        return f.result()

    def check_for_ddl_error(self, result):
        if not result["success"]:
            raise DataDefinitionLanguageError(result["error_code"], result["message"])

    def create_database(self, name: str, options=None):
        """
        Create a new database.

        :param name: The name of the database to create.
        :param options: Options for the database.
        :return: A database object for the new db.
        """
        r = self.cmd("s/" + name, payload=options, method="PUT")
        self.check_for_ddl_error(r)
        return db.Database(self, name, r["schema"])

    def open_database(self, name: str):
        """
        Open an existing database.

        :param name: The name of the database to open.
        :return: A database object for the db.
        """
        r = self.cmd("s/" + name, method="GET")
        self.check_for_ddl_error(r)
        return db.Database(self, name, r["schema"])

    def databases(self):
        """
        Retrieve a list of databases available on this cluster.
        :return: A list of database names.
        """
        r = self.cmd("s", method="GET")
        self.check_for_ddl_error(r)
        return r["schema"]


def connect(protocol="http", host="localhost", port=8080):
    return Connection(protocol=protocol, host=host, port=port)
