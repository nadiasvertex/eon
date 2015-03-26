module Store.Table where

import Data.Dynamic
import Data.Int

import Engine.Query
import Store.DataColumn
import Store.FrozenDataColumn

data ValueType = SmallInt | StdInt | BigInt |
                 Decimal | SmallSerial | StdSerial | BigSerial |
                 VarChar

data Column = Column {
  name         :: String,
  data_type    :: ValueType,
  frozen_data  :: Dynamic,
  flowing_data :: Dynamic
}

data Table =  Table {
  columns   :: [Column]
}
