module QueryParser where

-- System modules
import Control.Monad
import Text.ParserCombinators.Parsec

-- Eon modules
import Compute

primitives = [("+", (BinOpNode Add)),
              ("-", (BinOpNode Sub)),
              ("*", (BinOpNode Mul)),
              ("/", (BinOpNode Div)),
              ("!=", (BinOpNode Ne)),
              ("<=", (BinOpNode Le)),
              (">=", (BinOpNode Ge)),
              ("=", (BinOpNode Eq)),
              ("<", (BinOpNode Lt)),
              (">", (BinOpNode Gt))]

operation :: Parser String
operation =    (try string "!=")
           <|> (try string "<=")
           <|> (try string ">=")

parseBinaryExpr :: Parser Node
parseBinaryExpr  = 
  do
    left  <- parseExpr
    op    <- operation
    right <- parseExpr
    return BinOp $ op left right

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

parseLeaf :: Parser ComputeVal
parseLeaf = parseString
        <|> parseNumber
        <|> parseColumnRef

parseExpr :: Parser Node
parseExpr = liftM Leaf parseLeaf

readExpr :: String -> String
readExpr input = case parse parseExpr "query" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val
