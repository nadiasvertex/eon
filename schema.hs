module Schema (Table, Column, ColumnType) where

import Control.Monad.State
import Data.List
import Data.Maybe

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

get_column_with_name :: String -> [Column] -> Maybe Column
get_column_with_name name column_list = find (is_column_name name) column_list

getColumnType :: String -> [Column] -> Maybe ColumnType
getColumnType name column_list = let the_column = get_column_with_name name column_list
                                 in case the_column of
                                    Nothing   -> Nothing
                                    otherwise -> Just (columnType (fromJust the_column))

