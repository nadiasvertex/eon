{-# LANGUAGE TemplateHaskell #-}

module RowColumn where

import           Control.Lens
import           Data.Int            (Int64)
import qualified Data.Map as Map
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

-- The value of rows

data Row = Row {
  _rid      :: Int64,           -- The row id.
  _version  :: Int64,           -- The version of this row.
  _columns  :: VU.Vector Int64, -- The addresses of the columns for this row.
  _present  :: VU.Vector Bool   -- Which columns are present in this row.
} deriving (Show)
makeLenses ''Row

instance Eq Row where
  Row{_rid=r1, _version=v1} == Row{_rid=r2, _version=v2} = (r1==r2) && (v1==v2)

instance Ord Row where
  Row{_rid=r1, _version=v1} < Row{_rid=r2, _version=v2}
    | r1==r2    = v1<v2
    | r1<r2     = True
    | otherwise = False

-- The row column

data RowColumn = RowColumn {
  rows :: Map.Map Int64 Row
}

append RowColumn{rows=old_rows} rid version present values =
   RowColumn{rows=new_rows}
   where
      new_rows = Map.insert rid new_row old_rows
      new_row  = Row{
         _rid    =rid,
         _version=version,
         _columns=values,
         _present=present
      }
