module RowColumn where

import           Data.Int            (Int64)
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

-- | Identifies how to copy columns when updating rows.
data CopyColumn = None | Old | New

-- | The value of rows
data Row = Row {
  rid     :: Int64,           -- The row id.
  version :: Int64,           -- The version of this row.
  columns :: VU.Vector Int64, -- The addresses of the columns for this row.
  present :: VU.Vector Bool   -- Which columns are present in this row.
} deriving (Show)

instance Eq Row where
  Row{rid=r1, version=v1} == Row{rid=r2, version=v2} = (r1==r2) && (v1==v2)

instance Ord Row where
  Row{rid=r1, version=v1} < Row{rid=r2, version=v2}
    | r1==r2    = v1<v2
    | r1<r2     = True
    | otherwise = False

-- | The row column
data RowColumn = RowColumn {
  rows :: Map.Map Int64 [Row]
}

-- | Append a row to the database. The caller must ensure that the version is
--   unique.
--
--       rid: The row identifier of the new row.
--   version: The version of the new row.
--   present: Indicates what columns are present in this row.
--    values: The addresses of the values for the new columns.

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

-- | Update a row in the database. This will make a copy of the pointers to the
--   previous row, and then update them with the pointers in the current row.
--
--                rid: The row id for the row being updated.
--   previous_version: The version of the row being updated.
--            version: The new version of the row.
--
--            present: The 'present' field is special for update. If a value of
--   'Just true' is found, then the values field will have an updated value for that column. If
--   the value is 'Just false' then there is no update for that column for this
--   row. If it is present in the previous version, we copy that value over. If
--   it wasn't present, then we do nothing. Finally if the entry is 'Nothing'
--   then this column is being set to NULL. If there was a value in the previous
--   version of the row we omit it in this version. If there was no value in the
--   previous version then we ignore it.
--
--         new_values: The oids of the values of the columns.
--
update :: RowColumn          -> Int64 -> Int64         -> Int64 -> [Maybe Bool] -> VU.Vector Int64 -> RowColumn
update RowColumn{rows=old_rows} rid      previous_version version  new_present     new_values =
   RowColumn{rows=new_rows}
   where
      new_rows = Map.insert rid new_row_versions old_rows

      match_row Row{version=current_version} =
         current_version == previous_version

      old_row_versions = fromMaybe [] (Map.lookup rid old_rows)
      old_row_version  = head $ filter match_row old_row_versions

      new_row_versions = new_row_version : old_row_versions
      new_row_version  = Row {
         rid    =rid,
         version=version,
         columns=new_values,
         present=new_presence
      }

      old_values        = columns old_row_version
      old_present       = VU.toList $ present old_row_version
      combined_presence = zip [0..] $ zip new_present old_present
      fused_presence    = map map_presence combined_presence
      new_presence      = VU.fromList $ map match_presence  fused_presence
      copy_program      = filter match_presence fused_presence

      -- Figure out where the final column value comes from.
      map_presence (ix, (Nothing,       _)) = (ix, None)
      map_presence (ix, (Just True,     _)) = (ix,  New)
      map_presence (ix, (Just False, True)) = (ix,  Old)

      -- Determine which columns can be ignored

      match_presence (_, None) = False
      match_presence (_,    _) = True

      -- Copy the column values from the right location
      copy_column (ix, Old) = old_values VU.! ix
      copy_column (ix, New) = new_values VU.! ix
      copy (x:xs) = copy_column x : copy xs

      new_values  = VU.fromList $ reverse $ copy copy_program

-- | Find the row with the given id and version.
lookup :: RowColumn -> Int64 -> Int64 -> Maybe Row
lookup    row_column   rid      rversion =
  do
    row_versions  <- Map.lookup rid (rows row_column)
    last_version  <- listToMaybe $ reverse $ filter match_version row_versions
    return last_version
  where
    match_version Row{version=cversion} = rversion == cversion
