module Main (main) where

import System.Environment
import Compute
import Compute.RowStore
import QueryParser


main :: IO ()
main = do
         args <- getArgs
         print (parseString (head args))
