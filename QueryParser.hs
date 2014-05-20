module QueryParser where

-- System modules
import Control.Monad
import Text.ParserCombinators.Parsec

-- Eon modules
import Compute

{-
operation :: Parser Char
operation = oneOf "%&|*+-/"
-}

symbol :: Parser Char
symbol = oneOf "_"

parseIdent :: Parser String
parseIdent =
   do
      first <- letter <|> symbol
      rest <- many (letter <|> digit <|> symbol)
      let ident = first:rest
      return $ ident

parseSimpleRef :: Parser ComputeVal
parseSimpleRef =
   do
      c <- parseIdent
      return $ ColumnRef $ ColumnRefInfo "" c

parseFullyQualifiedRef :: Parser ComputeVal
parseFullyQualifiedRef =
   do
      t <- parseIdent
      char '.'
      c <- parseIdent
      return $ ColumnRef $ ColumnRefInfo t c

parseColumnRef :: Parser ComputeVal
parseColumnRef = try parseFullyQualifiedRef <|> parseSimpleRef

parseNumber :: Parser ComputeVal
parseNumber = liftM (Number . read) $ many1 digit

parseStringEl :: Parser String
parseStringEl =
	do
      char '\''
      x <- many (noneOf "'")
      char '\''
      return x

parseString :: Parser ComputeVal
parseString =
	do
		x  <- liftM (foldl1 (++)) $ many1 parseStringEl
		return $ String x

parseExpr :: Parser ComputeVal
parseExpr = parseString
        <|> parseNumber
        <|> parseColumnRef

readExpr :: String -> String
readExpr input = case parse parseExpr "query" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
