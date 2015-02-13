module Store.FrozenDataColumn where

import Data.Array.Repa
import Data.List        (sort, unzip)

import qualified Store.DataColumn as MD

data SegmentMap = SegmentMap {
  extents :: [MD.Extent],
  indexes :: Array U DIM1 Int
}

data FrozenSegment a = FrozenSegment {
    column   :: Array U DIM1 a
} deriving(Eq, Show)

freeze :: MD.Segment a -> FrozenSegment a
freeze             seg  =
  FrozenSegment{column=frozen_array}
  where
    size = length arr
    frozen_array   = fromListUnboxed (ix1 size) values
    (values, oids) = unzip sorted
    sorted = sort $ MD.enumerateSegment seg
    MD.Segment{MD.extents=xt, MD.array=arr} = seg
