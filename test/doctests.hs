module Main where
import Test.DocTest

main :: IO ()
main = doctest [
    "src/Store/DataColumn.hs",
    "src/Store/FrozenDataColumn.hs",
    "src/Store/RowColumn.hs",
    "src/Engine/Query.hs"
  ]
