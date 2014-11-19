from engine.column.storage.columnar.rtype import dt_to_tc

__author__ = 'Christopher Nelson'


class Membase:
    def __init__(self, data_type):
        self.data_type = data_type
        self.type_code = dt_to_tc.get(data_type)
        self.element_limit = 5000
