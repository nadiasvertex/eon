import System.Environment
import Compute
import Compute.RowStore
import QueryParser


main :: IO ()
main = do
         args <- getArgs
         putStrLn $ show (parseString (args !! 0))
