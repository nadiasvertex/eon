module QueryParser where

-- System modules
import Text.ParserCombinators.Parsec

-- Eon modules
import Compute

operation :: Parser Char
operation = oneOf "%&|*+-/"

parseStringEl :: Parser String
parseStringEl = 
	do 
		char "'"
        x <- many (noneOf "'")
        char "'"
        return $ String x

parseString :: Parser ComputeVal
parseString = 
	do
		xs <- many parseStringEl
		x  <- foldl1 (++) xs
		return $ String x  

parseExpr :: Parser LispVal
parseExpr = parseString
         <|> operation		

readExpr :: String -> String
readExpr input = case parse parseString "query" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"