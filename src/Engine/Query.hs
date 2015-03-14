module Engine.Query where

import           Data.Int (Int64)

class Query a where
  filter  :: a b -> (b -> Bool) -> [b]
  ifilter :: a b -> (b -> Bool) -> [(b, Int64)]
