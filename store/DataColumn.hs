module DataColumn where

import Data.Int (Int64, Int16)

data Extent = Extent {
    start   :: Int64,
    len     :: Int16
} deriving(Eq, Show)

data Segment a = Segment {
    extents    :: [Extent],
    array      :: [a]
}

appendExtent :: Int64 -> [Extent] -> [Extent]
appendExtent    oid      extents  =
    if oid - (xt_start + fromIntegral xt_length) == 1
    then Extent {start=xt_start, len=xt_length+1} : the_rest
    else Extent {start=oid, len=0}      : current : the_rest
    where
        (current:the_rest) = extents
        Extent {start=xt_start, len=xt_length} = current

enumerateExtent :: [    Extent   ] -> [Int64]
enumerateExtent    (last:the_rest) =
   enumerate_extent' (initial_oid xt_start xt_length) last the_rest
   where
      xt_start        = DataColumn.start last
      xt_length       = DataColumn.len   last
      initial_oid s l = s + fromIntegral l - 1

      enumerate_extent' current_value current_extent remaining_extents =
         current_value : if not has_more
                         then []
                         else enumerate_extent' next_value next_extent final_extents
         where
            Extent{start=current_start, len=_} = current_extent
            current_has_more                   = current_value-1 >= current_start
            has_more                           = current_has_more || remaining_extents /= []
            (next_value, next_extent, final_extents)
               | current_has_more              = (current_value-1, current_extent, remaining_extents)
               | otherwise                     =
                  let (x:xs) = remaining_extents
                      Extent{start=s, len=l} = x in
                      (initial_oid s l, x, xs)
