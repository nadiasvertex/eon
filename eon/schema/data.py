__author__ = 'Christopher Nelson'

import enum


@enum.unique
class DataType(enum.Enum):
    small_int = 1
    standard_int = 2
    big_int = 3
    varchar = 4
    decimal = 5
    bool = 6
