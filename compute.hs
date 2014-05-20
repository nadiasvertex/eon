module Compute where

import Schema

data ColumnRefInfo = ColumnRefInfo {
	table	:: Schema.Table,
	column  :: Schema.Column
}

data ComputeVal = ColumnRef ColumnRefInfo
             | List [ComputeVal]
             | Number Integer
             | String String
             | Bool Bool

