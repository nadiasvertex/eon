module Compute where

data ColumnRefInfo = ColumnRefInfo {
	table	:: String,
	column  :: String
}

data ComputeVal = ColumnRef ColumnRefInfo
             | List [ComputeVal]
             | Number Integer
             | String String
             | Bool Bool

