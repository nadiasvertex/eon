module Store.FrozenDataColumn where

import           Data.Array.Repa              (Array, DIM1, U, fromListUnboxed,
                                               ix1)
import           Data.Array.Repa.Repr.Unboxed (Unbox)

import           Data.Int                     (Int64)
import           Data.List                    (sortBy)
import           Data.Ord                     (comparing)

import qualified Store.DataColumn             as MD

data SegmentMap = SegmentMap {
  extents :: [MD.Extent],
  indexes :: Array U DIM1 Int64
} deriving (Eq, Show)

data FrozenSegment a = FrozenSegment {
    column :: Array U DIM1 a
} deriving(Eq, Show)

-- | Takes a mutable segment and turns it into a sorted, efficiently packed
-- immutable segment, plus a map indicating how to find the original oid in
-- the sorted array.
freeze :: (Ord a, Unbox a) => MD.Segment a -> (SegmentMap, FrozenSegment a)
freeze                               seg    =
  (SegmentMap    {extents=xt, indexes=frozen_indexes},
   FrozenSegment {column=frozen_array})
  where
    size           = length arr
    frozen_array   = fromListUnboxed (ix1 size) values
    frozen_indexes = fromListUnboxed (ix1 size) (reverse oids)
    (values, oids) = unzip sorted
    sorted         = sortBy (comparing fst) (MD.enumerateSegment seg)
    MD.Segment{MD.array=arr, MD.extents=xt} = seg
