__author__ = 'Christopher Nelson'

# We currently use monetdb as our cold store. This is very efficient for large tables, especially archival storage.

import monetdb
import os
import subprocess

class ManagementDriver:
    def __init__(self):
        self.host = "localhost"
        self.monetdb = subprocess.check_output("which monetdb", shell=True, universal_newlines=True).strip()
        self.monetdbd = subprocess.check_output("which monetdbd", shell=True, universal_newlines=True).strip()
        self.farm = "/opt/eon/var/cold_store"
        self.run = "/opt/eon/run"
        self.pidfile = os.path.join(self.run, "cold_store.pid")
        self.port = "30000"

        if not os.path.exists(self.farm):
            subprocess.check_call([self.monetdbd, "create", self.farm])

        subprocess.check_call([self.monetdbd, "set", "pidfile="+self.pidfile, self.farm])
        subprocess.check_call([self.monetdbd, "set", "port="+self.port, self.farm])
        if not os.path.exists(self.pidfile):
            subprocess.check_call([self.monetdbd, "start", self.farm])

    def create(self, new_database_name):
        cmd = [self.monetdb, "-q", "-p", self.port, "create", new_database_name]
        subprocess.check_call(cmd)

    def drop(self, existing_database_name):
        cmd = [self.monetdb, "-q", "-p", self.port, "destroy", "-f", existing_database_name]
        subprocess.check_call(cmd)

    def get_databases(self):
        cmd = [self.monetdb, "-q", "-p", self.port, "status"]
        listing = subprocess.check_output(cmd, universal_newlines=True)
        if not listing.strip():
            return None

        return [line.split(" ", 1)[0] for line in listing.splitlines()]

    def status(self, database_name):
        databases = self.get_databases()
        if databases is None or database_name not in databases:
            return None

        cmd = [self.monetdb, "-q", "-p", self.port, "status", "-sbrl", database_name]
        listing = subprocess.check_output(cmd, universal_newlines=True)
        if not listing.strip():
            return None

        lines = listing.splitlines()
        name, state, health, remarks = lines[1].split(" ", 3)
        return {"name": name, "state": state, "health": health, "remarks": remarks}






