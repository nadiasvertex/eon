import bisect
from array import array
from engine.schema.datatype import DataType

__author__ = 'Christopher Nelson'


class ResultType:
    ROW_ID = 0
    VALUE = 1

dt_to_tc = {
    DataType.i8 : 'b',
    DataType.i16: 'h',
    DataType.i32: 'i',
    DataType.i64: 'q',
    DataType.f32: 'f',
    DataType.f64: 'd'
}

class Membase:
    def __init__(self, data_type):
        self.data_type = data_type
        self.typecode = dt_to_tc.get(data_type)
        self.element_limit = 5000
