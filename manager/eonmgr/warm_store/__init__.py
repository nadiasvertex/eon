__author__ = 'cnelson'

import subprocess

class ManagementDriver:
    def __init__(self):
        self.host = "localhost"
        self.createdb = subprocess.check_output(["which", "createdb"], shell=True, universal_newlines=True)
        self.dropdb = subprocess.check_output(["which", "dropdb"], shell=True, universal_newlines=True)

    def create(self, new_database_name):
        cmd = [self.createdb, new_database_name]
        subprocess.check_call(cmd)

    def drop(self, existing_database_name):
        cmd = [self.dropdb, existing_database_name]
        subprocess.check_call(cmd)
