module QueryParser where

-- System modules
import Control.Monad
import Text.ParserCombinators.Parsec

-- Eon modules
import Compute

op_ne :: Parser (Node -> Node -> BinOpNode)
op_ne = do
   string "!="
   return $ (BinOpNode Ne)

op_le :: Parser (Node -> Node -> BinOpNode)
op_le = do
   string "<="
   return $ (BinOpNode Le)

op_ge :: Parser (Node -> Node -> BinOpNode)
op_ge = do
   string ">="
   return $ (BinOpNode Ge)

op_gt :: Parser (Node -> Node -> BinOpNode)
op_gt = do
   char '>'
   return $ (BinOpNode Gt)

op_lt :: Parser (Node -> Node -> BinOpNode)
op_lt = do
   char '<'
   return $ (BinOpNode Lt)

op_eq :: Parser (Node -> Node -> BinOpNode)
op_eq = do
   char '='
   return $ (BinOpNode Eq)

op_add :: Parser (Node -> Node -> BinOpNode)
op_add = do
   char '+'
   return $ (BinOpNode Add)

op_sub :: Parser (Node -> Node -> BinOpNode)
op_sub = do
   char '-'
   return $ (BinOpNode Sub)

op_mul :: Parser (Node -> Node -> BinOpNode)
op_mul = do
   char '+'
   return $ (BinOpNode Mul)

op_div :: Parser (Node -> Node -> BinOpNode)
op_div = do
   char '/'
   return $ (BinOpNode Div)

operation :: Parser (Node -> Node -> BinOpNode)
operation = try op_ne <|> try op_le <|> try op_ge
        <|> try op_lt <|> try op_gt <|> try op_eq
        <|> try op_add <|> try op_sub <|> try op_mul
        <|> op_div

parseBinaryExpr :: Parser Node
parseBinaryExpr  =
  do
    left  <- parseExpr
    op    <- operation
    right <- parseExpr
    return $ BinOp (op left right)

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
        <|> parseBinaryExpr

readExpr :: String -> String
readExpr input = case parse parseExpr "query" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val
