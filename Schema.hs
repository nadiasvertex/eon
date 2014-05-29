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

data Database = Database {
   tables     :: [Table]
}

is_column_name :: String -> Column -> Bool
is_column_name name column = columnName column == name

alter_column_type :: String -> ColumnType -> Column -> Column
alter_column_type name new_column_type column =
   if is_column_name name column then
      Column (columnId column) (columnName column) new_column_type else
      column

alter_column_name :: String -> String -> Column -> Column
alter_column_name name new_column_name column =
   if is_column_name name column then
      Column (columnId column) new_column_name (columnType column) else
      column

getColumnByName :: String -> [Column] -> Maybe Column
getColumnByName name = find (is_column_name name)

getColumnType :: String -> [Column] -> Maybe ColumnType
getColumnType name column_list =
   let the_column = getColumnByName name column_list
   in case the_column of
      Nothing   -> Nothing
      Just column -> Just (columnType column)

alterColumnType :: String -> ColumnType -> [Column] -> [Column]
alterColumnType name new_column_type = fmap (alter_column_type name new_column_type)

alterColumnName :: String -> String -> [Column] -> [Column]
alterColumnName name new_column_name = fmap (alter_column_name name new_column_name)
