module RowColumn where

import           Data.Int            (Int64)
import           Data.List           (mapAccumL)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Vector.Unboxed ((!))
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
} deriving (Show)

-- | Transforms a present list that can have nulled values into a lookup array
--   for the column values.
--
--   Example:
--   [Just False, Just True]
--   [(0, False),  (1,True)]
--   [        -1,         0]
--
--   When we use got to copy from the columns array we know that absolute
--   column 1 is actually in the columns array at offset 0. A value of -1
--   means not present.
presentToIndexWithNull :: [Maybe Bool] -> VU.Vector Int
presentToIndexWithNull  =
  VU.fromList . snd . mapAccumL accum 0
  where
    accum acc el = if convert_present el then (acc+1, acc) else (acc, -1)

    convert_present Nothing      = False
    convert_present (Just False) = False
    convert_present (Just  True) = True

-- | Transforms a frozen present list into a lookup array for the column
--   values. This is the array equivalent of presentToIndexWithNull
--   above.
presentToIndex :: VU.Vector Bool -> VU.Vector Int
presentToIndex present  =
    VU.map mapper truth_indexes
  where
    accum acc el         = if el then acc+1 else acc
    mapper (present, ix) = if present then ix else -1
    truth_indexes        = VU.zip present indexes
    indexes              = VU.prescanl accum 0 present

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
-- 'Just true' is found, then the values field will have an updated value for
-- that column. If the value is 'Just false' then there is no update for that
-- column for this row. If it is present in the previous version, we copy that
-- value over. If it wasn't present, then we do nothing. Finally if the entry is
-- 'Nothing' then this column is being set to NULL. If there was a value in the
-- previous version of the row we omit it in this version. If there was no value
-- in the previous version then we ignore it.
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
         columns=final_values,
         present=updated_present
      }

      old_present_indexes = presentToIndex (present old_row_version)
      new_present_indexes = presentToIndexWithNull new_present

      old_values        = columns old_row_version
      old_present       = VU.toList $ present old_row_version
      updated_present   = VU.fromList $ map match_presence fused_presence
      combined_presence = zip [0..] $ zip new_present old_present
      fused_presence    = map map_presence combined_presence
      copy_program      = filter match_presence fused_presence

      -- Figure out where the final column value comes from.
      map_presence (ix, (Nothing,       _)) = (ix, None)
      map_presence (ix, (Just True,     _)) = (ix,  New)
      map_presence (ix, (Just False, True)) = (ix,  Old)

      -- Determine which columns can be ignored
      match_presence (_, None) = False
      match_presence (_,    _) = True

      -- Copy the column values from the right location
      copy_column (ix, Old) = old_values ! (old_present_indexes ! ix)
      copy_column (ix, New) = new_values ! (new_present_indexes ! ix)

      final_values  = VU.fromList $ reverse $ map copy_column copy_program

{- | Note on Update |

 The update process is fairly straight forward, but there are some complexities.
Basically, the process works like this:

  1. Find the previous version of the row.
  2. Determine the mapping between absolute column index and the stored column
    index.
  3. Evaluate the new version of the row.
  4. Determine the mapping between the absolute and stored column indexes for
    the new row.

  Old Version:
  [    0,     1,      2,     3,     4,    5]
  [False, False,   True,  True, False, True]
  [   -1,    -1,      0,    -1,    -1,    1] = result of step 2


  New Version:
  [    0,     1,       2,    3,     4,    5]
  [False, False, Nothing, True, False, True]
  [   -1,    -1,      -1,    0,    -1,    1] = result of step 4

  5. Determine what columns will actually be present in the new row.
  6. Create a list of copy operations that should be performed to generate
    a new row.
  7. Generate a new column array by executing the list of copy operations.

-}

-- | Find the row with the given id and version.
lookup :: RowColumn -> Int64 -> Int64 -> Maybe Row
lookup    row_column   rid      rversion =
  do
    row_versions  <- Map.lookup rid (rows row_column)
    last_version  <- listToMaybe $ reverse $ filter match_version row_versions
    return last_version
  where
    match_version Row{version=cversion} = rversion == cversion
