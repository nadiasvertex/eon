-- | Stores the row column, which provides a way to locate the address of
--   column values for any particular row. It also handles row versions
--   and null values.
module Store.RowColumn where

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
  rid     :: Int64,           -- ^ The row id.
  version :: Int64,           -- ^ The version of this row.
  columns :: VU.Vector Int64, -- ^ The addresses of the columns for this row.
  present :: VU.Vector Bool   -- ^ Which columns are present in this row.
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
  rows :: Map.Map Int64 [Row] -- ^ A map of lists of rows, indexed by row id.
} deriving (Eq, Show)

-- | Transforms a present list that can have nulled values into a lookup array
--   for the column values.
--
--   @
--   Example:
--   [Just False, Just True]
--   [(0, False),  (1,True)]
--   [        -1,         0]
--   @
--
--   When we use it to copy from the columns array we know that absolute
--   column 1 is actually in the columns array at offset 0. A value of -1
--   means not present.
--
-- >>> let p1 = [Just False, Just False, Nothing, Just True, Just False, Just True]
-- >>> presentToIndexWithNull p1
-- fromList [-1,-1,-1,0,-1,1]

presentToIndexWithNull :: [Maybe Bool]  -- ^ The present list
                       -> VU.Vector Int -- ^ The lookup array.

presentToIndexWithNull  =
  VU.fromList . snd . mapAccumL accum 0
  where
    accum acc el = if convert_present el then (acc+1, acc) else (acc, -1)

    convert_present Nothing      = False
    convert_present (Just False) = False
    convert_present (Just  True) = True

-- | Transforms a frozen present vector into a lookup array for the column
--   values. This is the array equivalent of presentToIndexWithNull
--   above.
--
-- >>> let p2 = VU.fromList [False, False, True, True, False, True]
-- >>> presentToIndex p2
-- fromList [-1,-1,0,1,-1,2]
presentToIndex :: VU.Vector Bool -- ^ The frozen present vector.
               -> VU.Vector Int  -- ^ The lookup array.
presentToIndex present_arr  =
    VU.map mapper truth_indexes
  where
    accum acc el            = if el then acc+1 else acc
    mapper (is_present, ix) = if is_present then ix else -1
    truth_indexes           = VU.zip present_arr indexes
    indexes                 = VU.prescanl accum 0 present_arr

-- | Append a row to the database. The caller must ensure that the version is
--   unique.
--
--   [@row_id@]       The row identifier of the new row.
--
--   [@row_version@]  The version of the new row.
--
--   [@col_presence@] Indicates what columns are present in this row.
--
--   [@values@]       The addresses of the values for the new columns.
--
-- >>> let rc = RowColumn{rows=Map.empty}
-- >>> append rc 1 1 (VU.fromList [True, False]) (VU.fromList [10])
-- RowColumn {rows = fromList [(1,[Row {rid = 1, version = 1, columns = fromList [10], present = fromList [True,False]}])]}
--
-- >>> let rc = RowColumn{rows=Map.empty}
-- >>> let rc2 = append rc 1 1 (VU.fromList [True, False]) (VU.fromList [10])
-- >>> append rc2 1 2 (VU.fromList [True, False]) (VU.fromList [20])
-- RowColumn {rows = fromList [(1,[Row {rid = 1, version = 2, columns = fromList [20], present = fromList [True,False]},Row {rid = 1, version = 1, columns = fromList [10], present = fromList [True,False]}])]}

append :: RowColumn       -- ^ The rows to append to.
       -> Int64           -- ^ row_id
       -> Int64           -- ^ row_version
       -> VU.Vector Bool  -- ^ col_presence
       -> VU.Vector Int64 -- ^ values
       -> RowColumn       -- ^ The new rows.

append RowColumn{rows=old_rows} rid' version' present' values =
   RowColumn{rows=new_rows}
   where
      new_rows         = Map.insert rid' new_row_versions old_rows
      old_row_versions = fromMaybe [] (Map.lookup rid' old_rows)

      new_row_versions = new_row_version : old_row_versions
      new_row_version  = Row{
         rid    =rid',
         version=version',
         columns=values,
         present=present'
      }

-- | Update a row in the database. This will make a copy of the pointers to the
--   previous row, and then update them with the pointers in the current row.
--
--  [@row_id@] The row id for the row being updated.
--
--  [@previous_version@] The version of the row being updated.
--
--  [@version_to_update@] The new version of the row.
--
--  [@updates_present@] The 'updates_present' field is special for update. If a
-- value  of  'Just true' is found, then the values field will have an updated
-- value for  that  column. If the value is 'Just false' then there is no update
-- for that  column  for this row. If it is present in the previous version, we
-- copy that  value  over. If it wasn't present, then we do nothing. Finally if the
-- entry is  'Nothing' then this column is being set to NULL. If there was a value
-- in the  previous version of the row we omit it in this version. If there was no
-- value  in the previous version then we ignore it.
--
--  [@new_values@] The oids of the values of the columns.
--
--  The update process is fairly straight forward, but there are some complexities.
--  Basically, the process works like this:
--
--   1. Find the previous version of the row.
--
--   2. Determine the mapping between absolute column index and the stored column
--   index.
--
--   3. Evaluate the new version of the row.
--
--   4. Determine the mapping between the absolute and stored column indexes for
--   the new row.
--
--  @
--  Old Version:
--  [    0,     1,      2,     3,     4,    5]
--  [False, False,   True,  True, False, True]
--  [   -1,    -1,      0,    -1,    -1,    1] = result of step 2
--  @
--
--  @
--  New Version:
--  [    0,     1,       2,    3,     4,    5]
--  [False, False, Nothing, True, False, True]
--  [   -1,    -1,      -1,    0,    -1,    1] = result of step 4
--  @
--
--   5. Determine what columns will actually be present in the new row.
--
--   6. Create a list of copy operations that should be performed to generate
--   a new row.
--
--   7. Generate a new column array by executing the list of copy operations.
--
-- >>> let rc = RowColumn{rows=Map.empty}
-- >>> let rc2 = append rc 1 1 (VU.fromList [True, False]) (VU.fromList [10])
-- >>> let rc3 = append rc2 1 2 (VU.fromList [True, False]) (VU.fromList [20])
-- >>> update rc3 1 2 3 [Just False, Just True] (VU.fromList [130])
-- RowColumn {rows = fromList [(1,[Row {rid = 1, version = 3, columns = fromList [130,20], present = fromList [True,True]},Row {rid = 1, version = 2, columns = fromList [20], present = fromList [True,False]},Row {rid = 1, version = 1, columns = fromList [10], present = fromList [True,False]}])]}

update :: RowColumn       -- ^ The old rows.
       -> Int64           -- ^ row_id
       -> Int64           -- ^ previous_version
       -> Int64           -- ^ version_to_update
       -> [Maybe Bool]    -- ^ updates_present
       -> VU.Vector Int64 -- ^ new_values
       -> RowColumn       -- ^ The new rows.
update RowColumn{rows=old_rows} rid' previous_version version' new_present new_values =
   RowColumn{rows=new_rows}
   where
      new_rows = Map.insert rid' new_row_versions old_rows

      match_row Row{version=current_version} =
         current_version == previous_version

      old_row_versions = fromMaybe [] (Map.lookup rid' old_rows)
      old_row_version  = head $ filter match_row old_row_versions

      new_row_versions = new_row_version : old_row_versions
      new_row_version  = Row {
         rid    =rid',
         version=version',
         columns=final_values,
         present=updated_present
      }

      old_present_indexes = presentToIndex (present old_row_version)
      new_present_indexes = presentToIndexWithNull new_present

      old_values        = columns old_row_version
      old_present       = VU.toList $ present old_row_version
      updated_present   = VU.fromList $ fmap match_presence fused_presence
      combined_presence = zip [0..] $ zip new_present old_present
      fused_presence    = fmap map_presence combined_presence
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

      final_values  = VU.fromList . reverse $ fmap copy_column copy_program

-- | Find the row with the given id and version.
--
--  [@row_id@]      The row id to look for.
--
--  [@row_version@] The row version to look for.
--
-- >>> let rc1 = RowColumn{rows=Map.empty}
-- >>> let rc2 = append rc1 1 1 (VU.fromList [True, False]) (VU.fromList [10])
-- >>> let rc3 = append rc2 1 2 (VU.fromList [True, False]) (VU.fromList [20])
-- >>> let rc4 = update rc3 1 2 3 [Just False, Just True] (VU.fromList [130])
-- >>> Store.RowColumn.lookup rc4 1 2
-- Just (Row {rid = 1, version = 2, columns = fromList [20], present = fromList [True,False]})

lookup :: RowColumn -- ^ The rows to search.
       -> Int64     -- ^ row_id
       -> Int64     -- ^ row_version
       -> Maybe Row -- ^ @Just Row@ if the row and version is found, or @Nothing@.
lookup row_column rid' rversion =
  do
    row_versions  <- Map.lookup rid' (rows row_column)
    last_version  <- listToMaybe . reverse $ filter match_version row_versions
    return last_version
  where
    match_version Row{version=cversion} = rversion == cversion
