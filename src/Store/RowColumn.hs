module RowColumn where

import           Control.Lens
import           Data.Int            (Int64)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

-- The value of rows

data Row = Row {
  rid      :: Int64,           -- The row id.
  version  :: Int64,           -- The version of this row.
  columns  :: VU.Vector Int64, -- The addresses of the columns for this row.
  present  :: VU.Vector Bool   -- Which columns are present in this row.
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
  rows :: Map.Map Int64 [Row]
}

append :: RowColumn          -> Int64 -> Int64 -> VU.Vector Bool -> VU.Vector Int64 -> RowColumn
append RowColumn{rows=old_rows} rid      version  present           values =
   RowColumn{rows=new_rows}
   where
      new_rows         = Map.insert rid new_row_versions old_rows
      old_row_versions = fromMaybe [] (Map.lookup rid old_rows)

      new_row_versions = new_row_version : old_row_versions
      new_row_version  = Row{
         rid    =rid,
         version=version,
         columns=values,
         present=present
      }

update :: RowColumn          -> Int64 -> Int64         -> Int64 -> VU.Vector Bool -> VU.Vector Int64 -> RowColumn
update RowColumn{rows=old_rows} rid      previous_version version  present           values =
   RowColumn{rows=new_rows}
   where
      new_rows = Map.insert rid new_row_versions old_rows

      match_row Row{version=current_version} =
         current_version == previous_version

      old_row_versions = fromMaybe [] (Map.lookup rid old_rows)
      old_row_version  = head $ filter match_row old_row_versions

      new_row_versions = new_row_version old_row_version : old_row_versions
      new_row_version Row{columns=old_columns, present=old_present} = Row {
         rid    =rid,
         version=version,
         columns=values,
         present=present
      }
