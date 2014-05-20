module Schema (Table, Column, ColumnType) where

import Control.Monad.State
import Data.List

data ColumnType = TinyInteger
               |  SmallInteger
               |  StandardInteger
               |  BigInteger
               |  Decimal
               |  DateTime
               |  VarChar
               deriving (Eq, Show)

data Column = Column {
   columnId   :: Int,
   columnName :: String,
   columnType :: ColumnType
} deriving (Eq, Show)

data Table = Table {
   tableId    :: Int,
   tableName  :: String,
   columns    :: [Column]
} deriving (Eq, Show)

is_column_name :: String -> Column -> Bool
is_column_name name column = (columnName column) == name

alter_column_type :: String -> ColumnType -> Column -> Column
alter_column_type name new_column_type column =
   case (is_column_name name column) of
      True  -> Column (columnId column) (columnName column) new_column_type
      False -> column

alter_column_name :: String -> String -> Column -> Column
alter_column_name name new_column_name column =
   case (is_column_name name column) of
      True  -> Column (columnId column) new_column_name (columnType column)
      False -> column

getColumnByName :: String -> [Column] -> Maybe Column
getColumnByName name column_list = find (is_column_name name) column_list

getColumnType :: String -> [Column] -> Maybe ColumnType
getColumnType name column_list =
   let the_column = getColumnByName name column_list
   in case the_column of
      Nothing   -> Nothing
      Just column -> Just (columnType column)

alterColumnType :: String -> ColumnType -> [Column] -> [Column]
alterColumnType name new_column_type column_list = map (alter_column_type name new_column_type) column_list

alterColumnName :: String -> String -> [Column] -> [Column]
alterColumnName name new_column_name column_list = map (alter_column_name name new_column_name) column_list

