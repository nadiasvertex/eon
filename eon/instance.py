from eon.schema.store import Store

__author__ = 'Christopher Nelson'

_instance_store = None
def get_store():
    return _instance_store

def create_store(base_data_dir):
    global _instance_store
    _instance_store = Store(base_data_dir)

