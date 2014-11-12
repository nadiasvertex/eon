import bisect
from array import array

__author__ = 'Christopher Nelson'


class ResultType:
    ROW_ID = 0
    VALUE = 1




class Membase:
    def __init__(self, typecode):
        self.typecode = typecode
        self.element_limit = 5000
