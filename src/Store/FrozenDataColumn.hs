module Store.FrozenDataColumn where

import Data.Array.Repa
import Store.DataColumn

data FrozenSegment a = FrozenSegment {
    extents :: [Extent],
    array   :: Array U DIM1 a
} deriving(Eq, Show)
