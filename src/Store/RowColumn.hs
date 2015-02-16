module RowColumn where

import           Data.Int            (Int64)
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

-- The value of rows

data Row = Row {
  rid      :: Int64,           -- The id of this row.
  version  :: Int64,           -- The version of this row.
  columns  :: VU.Vector Int64, -- The addresses of the columns for this row.
  presence :: VU.Vector Bool   -- Which columns are present in this row.
} deriving (Show)

instance Eq Row where
  Row{rid=r1, version=v1} == Row{rid=r2, version=v2} = (r1==r2) && (v1==v2)

instance Ord Row where
  Row{rid=r1, version=v1} < Row{rid=r2, version=v2}
    | r1==r2    = v1<v2
    | r1<r2     = True
    | otherwise = False

-- The row column

data RowColumn = RowColumn {
  rows :: V.Vector Row
}

--append rid version columns values =
