from enum import Enum

__author__ = 'Christopher Nelson'


class Level(Enum):
    read_dirty = 0
    read_committed = 1
    snapshot = 2
    serializable = 3

