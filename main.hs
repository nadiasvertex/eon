import System.Environment
import Compute
import QueryParser

main :: IO ()
main = do 
         args <- getArgs
         putStrLn (readExpr (args !! 0))
