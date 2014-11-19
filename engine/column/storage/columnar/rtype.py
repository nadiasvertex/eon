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
