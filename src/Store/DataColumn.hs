module Store.DataColumn where

-- System modules
import           Data.Int     (Int16, Int64)

-- Application modules
import           Engine.Query

data Extent = Extent {
    start :: Int64,
    len   :: Int16
} deriving(Eq, Show)

data Segment a = Segment {
    extents :: [Extent],
    array   :: [a]
} deriving(Eq, Show)

instance Engine.Query.Query Segment where
  filter  = filterSegment
  ifilter = ifilterSegment

-- | Append a new oid to a list of extents.
appendExtent :: [Extent] -> Int64 -> [Extent]
appendExtent    [      ]    oid    = [Extent {start=oid, len=0}]
appendExtent      xts       oid    =
    if    oid - (xt_start + fromIntegral xt_length) == 1
       && xt_length < (maxBound :: Int16)
    then Extent {start=xt_start, len=xt_length+1} : the_rest
    else Extent {start=oid, len=0}      : current : the_rest
    where
        (current:the_rest) = xts
        Extent {start=xt_start, len=xt_length} = current

-- | Turn a list of extents into a series of numbers.
enumerateExtent :: [    Extent      ] -> [Int64]
enumerateExtent    (last_xt:the_rest) =
  enumerate_extent' (initial_oid xt_start xt_length) last_xt the_rest
  where
    xt_start        = Store.DataColumn.start last_xt
    xt_length       = Store.DataColumn.len   last_xt
    initial_oid s l = s + fromIntegral l

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
           | otherwise                     = let
              (x:xs) = remaining_extents
              Extent{start=s, len=l} = x in
                (initial_oid s l, x, xs)

-- | Take a list of oids and turn it into a list of extents.
extentFromList :: [Extent] -> [ Int64 ] -> [Extent]
extentFromList       xt       [       ] =  xt
extentFromList       xt       ( x:xs  ) =
   extentFromList r xs
   where
      r = appendExtent xt x


-- | Appends a value to a segment.
appendValue :: Segment a -> Int64 -> a -> Segment a
appendValue Segment{extents=xt, array=arr} oid value =
  Segment{extents=appendExtent xt oid, array=value : arr}

enumerateSegment :: Segment a -> [(a, Int64)]
enumerateSegment         seg   =
  zip arr (enumerateExtent xt)
  where
    Segment{extents=xt, array=arr} = seg

-- | Take a list of oids and values, and append them to a segment.
segmentFromList :: Segment a -> [(Int64, a)] -> Segment a
segmentFromList          seg    [          ] =  seg
segmentFromList          seg    (   x:xs   ) =
   segmentFromList r xs
   where
      (oid, value) = x
      r            = appendValue seg oid value

-- | Filter a segment using function 'f', returning a list of matching values
filterSegment :: Segment a    -- ^ The segment to filter
              -> (a -> Bool)  -- ^ The function to user as the filter
              -> [a]          -- ^ Return a list of values that pass the filter
filterSegment s f = flt (enumerateSegment s)
  where
    flt [        ] = []
    flt ((v,i):xs) = if f v
      then v : flt xs
      else flt xs

ifilterSegment s f = flt (enumerateSegment s)
  where
    flt [        ] = []
    flt ((v,i):xs) = if f v
      then (v,i) : flt xs
      else flt xs

-- | Create a new, empty segment
empty :: Segment a
empty = Segment {extents=[],  array=[]}
