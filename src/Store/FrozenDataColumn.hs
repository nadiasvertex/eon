{-# LANGUAGE DeriveDataTypeable #-}
module Store.FrozenDataColumn where

import           Data.Array.Repa              (Array, DIM1, U, fromListUnboxed,
                                               ix1)
import           Data.Array.Repa.Repr.Unboxed (Unbox)

import           Data.Int                     (Int64)
import           Data.List                    (sortBy)
import           Data.Ord                     (comparing)
import           Data.Typeable

import qualified Store.DataColumn             as MD

data SegmentMap = SegmentMap {
  extents :: [MD.Extent],
  indexes :: Array U DIM1 Int64
} deriving (Eq, Show)

data FrozenSegment a = FrozenSegment {
    column :: Array U DIM1 a
} deriving(Eq, Show, Typeable)

-- | Takes a mutable segment and turns it into a sorted, efficiently packed
-- immutable segment, plus a map indicating how to find the original oid in
-- the sorted array.
--
freeze :: (Ord a, Unbox a)
       => MD.Segment a                  -- ^ The segment to freeze
       -> (SegmentMap, FrozenSegment a) -- ^ Returns a frozen copy of the segment.

freeze seg    =
  (SegmentMap    {extents=xt, indexes=frozen_indexes},
   FrozenSegment {column=frozen_array})
  where
    size           = length arr
    frozen_array   = fromListUnboxed (ix1 size) values
    frozen_indexes = fromListUnboxed (ix1 size) (reverse oids)
    (values, oids) = unzip sorted
    sorted         = sortBy (comparing fst) (MD.enumerateSegment seg)
    MD.Segment{MD.array=arr, MD.extents=xt} = seg


-- | Takes a frozen segment and filters it according to the function given.
filterSegment :: FrozenSegment a  -- ^ The segment to filter.
              -> (a -> Bool)      -- ^ The function to filter by.
              -> Array U Dim1 Int -- ^ Returns an array of matching indexes.
filterSegment FrozenSegment{column=old_column} predicate =
  FrozenSegment{column = filtered_column }
  where
    filtered_column     = selectP apply_predicate id size
    apply_predicate idx = predicate $ old_column ! idx
    size                = length old_column
