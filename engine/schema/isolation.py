from enum import Enum

__author__ = 'Christopher Nelson'


class Level(Enum):
    read_dirty = 0
    read_committed = 1
    repeatable_read = 3
    snapshot = 4
    serializable = 5

