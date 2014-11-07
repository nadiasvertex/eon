__author__ = 'Christopher Nelson'

import enum


class DataType(enum.Enum):
    i8 = 0
    i16 = 1
    i32 = 2
    i64 = 3
    f32 = 4
    f64 = 5
    decimal = 6
    varchar = 7


