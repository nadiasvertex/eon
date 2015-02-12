module Store.FrozenDataColumn where

import Data.Array.Repa
import Store.DataColumn

data FrozenSegment a = FrozenSegment {
    extents :: [Extent],
    array   :: Array U DIM1 a
} deriving(Eq, Show)

freeze :: Segment a -> FrozenSegment a
freeze Segment { extents=xt, array=arr } =
   FrozenSegment { extents=xt, array=frozen_array }
   where
      frozen_array = fromListUnboxed arr
