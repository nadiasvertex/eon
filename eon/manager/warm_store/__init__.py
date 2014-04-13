__author__ = 'cnelson'

import subprocess


class ManagementDriver:
    def __init__(self):
        self.host = "localhost"
        self.createdb = subprocess.check_output("which createdb", shell=True, universal_newlines=True).strip()
        self.dropdb = subprocess.check_output("which dropdb", shell=True, universal_newlines=True).strip()
        self.psql = subprocess.check_output("which psql", shell=True, universal_newlines=True).strip()

    def create(self, new_database_name):
        cmd = [self.createdb, new_database_name]
        subprocess.check_call(cmd)

    def drop(self, existing_database_name):
        cmd = [self.dropdb, existing_database_name]
        subprocess.check_call(cmd)

    def get_databases(self):
        cmd = [self.psql, "-l"]
        listing = subprocess.check_output(cmd, universal_newlines=True)
        if not listing.strip():
            return None

        listing = listing.splitlines()
        return [line.split("|",1)[0].strip() for line in listing[3:]]

    def status(self, database_name):
        databases = self.get_databases()
        if database_name not in databases:
            return None

        cmd = [self.psql, "-l"]
        listing = subprocess.check_output(cmd, universal_newlines=True)
        if not listing.strip():
            return None

        listing = listing.splitlines()
        for line in listing[3:]:
            name, owner, encoding, collate, ctype, access = line.split("|")
            name = name.strip()
            if name == database_name:
                return {"name": name, "owner": owner.strip()}

        return None

    def create_user(self, database_name, user_name, password):
        query = "CREATE USER {user_name} WITH PASSWORD '{password}';".format(
            user_name = user_name,
            password = password
        )
        cmd = [self.psql, "-q", "-d", database_name, "-c", query]
        subprocess.check_call(cmd)

    def drop_user(self, database_name, user_name):
        query = "DROP ROLE IF EXISTS {user_name};".format(
            user_name = user_name,
        )
        cmd = [self.psql, "-q", "-d", database_name, "-c", query]
        subprocess.check_call(cmd)

