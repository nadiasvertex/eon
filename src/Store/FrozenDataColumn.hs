module Store.FrozenDataColumn where

import Data.Array.Repa (Array, U, DIM1, ix1, fromListUnboxed)
import Data.Array.Repa.Repr.Unboxed (Unbox)

import Data.List        (sortBy)
import Data.Ord         (comparing)

import qualified Store.DataColumn as MD

data SegmentMap = SegmentMap {
  extents :: [MD.Extent],
  indexes :: Array U DIM1 Int
}

data FrozenSegment a = FrozenSegment {
    column   :: Array U DIM1 a
} deriving(Eq, Show)

freeze :: (Ord a, Unbox a) => MD.Segment a -> FrozenSegment a
freeze             seg  =
  FrozenSegment{column=frozen_array}
  where
    size = length arr
    frozen_array   = fromListUnboxed (ix1 size) values
    (values, oids) = unzip sorted
    sorted = sortBy (comparing fst) (MD.enumerateSegment seg)
    MD.Segment{MD.array=arr} = seg
