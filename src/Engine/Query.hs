module Engine.Query where

import           Data.Conduit
import qualified Data.Conduit.List      as CL
import           Data.Int               (Int64)

import           Store.FrozenDataColumn

class Queryable a where
  filter  :: a b -> (b -> Bool) -> [b]
  ifilter :: a b -> (b -> Bool) -> [(b, Int64)]

-- | Take a frozen segment and run the predicate over it, producing a stream
-- | of segment indices that match the predicate.
filteredIndexSource seg predicate =
  loop 0
  where
    filtered = filterSegment seg predicate
    size     = length filtered

    loop idx
      | idx < size = do
          yield (filtered ! idx)
          loop $ idx+1

      | idx >= size = do return ()
