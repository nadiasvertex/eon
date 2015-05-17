"""
Stores the schema of all active databases.
"""
from datetime import datetime
import getpass
import hashlib
import json
import logging
import os
import random
import string
from eon.schema.db import Database

__author__ = 'Christopher Nelson'

NEW_PASSWORD_LENGTH = 10


class Store:
    def __init__(self, data_dir, default_values={}):
        self.log = logging.getLogger()
        self.data_dir = data_dir
        self.configuration = None
        self.databases = {}

        if not os.path.exists(data_dir):
            self.log.info("Creating data directory '%s'", data_dir)
            os.makedirs(data_dir)

        self.store_file = os.path.join(self.data_dir, "store.json")
        if not os.path.exists(self.store_file):
            new_password = default_values.get("password", self._pass_gen())
            initial_data = {
                "created": datetime.now().isoformat(),
                "modified": datetime.now().isoformat(),
                "admin": getpass.getuser(),
                "pass": hashlib.sha512(new_password.encode("utf-8")).hexdigest(),
                "databases": []
            }
            with open(self.store_file, "wb") as o:
                o.write(json.dumps(initial_data).encode("utf-8"))

            self.log.info("New data site initialized at '%s'", self.data_dir)

            if "password" not in default_values:
                self.log.info(
                    "Password for new instance is '% s'. You should change this as "
                    "soon as possible for production use.", new_password
                )

        self._load_site()

    def _pass_gen(self):
        """
        Generates a secure random password.
        :return: The new password.
        """

        r = random.SystemRandom()
        return ''.join(
            [r.choice(string.ascii_letters + string.digits + string.punctuation)
             for _ in range(NEW_PASSWORD_LENGTH)]
        )

    def _load_site(self):
        with open(self.store_file, "rb") as i:
            self.configuration = json.loads(i.read().decode("utf-8"))

        self.log.info("Loaded configuration from '%s'", self.store_file)
        for db in self.configuration["databases"]:
            dbo = Database()
            dbo.load(db)

            self.databases[dbo.name] = dbo
            self.log.debug("loaded '%s'", dbo.name)

    def get_database(self, name):
        return self.databases.get(name)



