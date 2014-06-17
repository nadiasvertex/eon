module Parse.Query where

-- System modules
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Eon modules
import Compute

languageDef =
   emptyDef {
      Token.commentStart         = "/*",
      Token.commentEnd           = "*/",
      Token.commentLine          = "--",
      Token.identStart           = letter   <|> char '_',
      Token.identLetter          = alphaNum <|> char '_',

      Token.reservedNames        = [ "from", "where", "select", "update", "insert",
                                     "values", "inner", "outer", "join", "as", "on",
                                     "in", "between", "and", "or", "not",
                                     "create", "drop", "alter", "table", "column", "grant",
                                     "true", "false"
                                   ],

      Token.reservedOpNames      = ["+", "-", "*", "/", ">", "<", "=", "<>", "!=", "<=", ">="]
}

lexer = Token.makeTokenParser languageDef

identifier   = Token.identifier lexer  -- parses an identifier
reserved     = Token.reserved   lexer  -- parses a reserved name
reserved_op  = Token.reservedOp lexer  -- parses an operator
parens       = Token.parens     lexer  -- parses surrounding parenthesis
integer      = Token.integer    lexer  -- parses an integer
semi         = Token.semi       lexer  -- parses a semicolon
white_space  = Token.whiteSpace lexer  -- parses whitespace

arith_operators = [
   [Prefix (reserved_op "-"   >> return  Neg                  )          ],
   [Infix  (reserved_op "*"   >> return (ArithBinary Multiply)) AssocLeft],
   [Infix  (reserved_op "/"   >> return (ArithBinary Divide  )) AssocLeft],
   [Infix  (reserved_op "+"   >> return (ArithBinary Add     )) AssocLeft],
   [Infix  (reserved_op "-"   >> return (ArithBinary Subtract)) AssocLeft]
                  ]

bool_operators = [
   [Prefix (reserved_op "not" >> return  Not                 )          ],
   [Infix  (reserved_op "and" >> return (BoolBinary And     )) AssocLeft],
   [Infix  (reserved_op "or"  >> return (BoolBinary Or      )) AssocLeft]
                 ]

qualified_identifier =
   do
      table  <- identifier
      char '.'
      column <- identifier
      return $ ColumnRef table column

arith_term = parens arith_expr
         <|> try qualified_identifier
         <|> fmap (ColumnRef "") identifier
         <|> fmap IntConst integer

bool_term  = parens bool_expr
         <|> (reserved "true"  >> return (BoolConst True ))
         <|> (reserved "false" >> return (BoolConst False))
         <|> rel_expr

relation   = (reserved_op "="  >> return JustEqual)
         <|> (reserved_op ">"  >> return GreaterThan)
         <|> (reserved_op "<"  >> return LessThan)
         <|> (reserved_op "<=" >> return LessOrEqual)
         <|> (reserved_op ">=" >> return GreaterOrEqual)
         <|> (reserved_op "!=" >> return NotEqual)
         <|> (reserved_op "<>" >> return NotEqual)

arith_expr :: Parser ArithExpr
arith_expr = buildExpressionParser arith_operators arith_term

bool_expr :: Parser BoolExpr
bool_expr = buildExpressionParser bool_operators bool_term

rel_expr =
   do
      a1 <- arith_expr
      op <- relation
      a2 <- arith_expr
      return $ RelBinary op a1 a2

table_ref =
   do
      table <- identifier
      alias <- optionMaybe (
         do
            reserved "as"
            alias_name <- identifier
            return alias_name
         )
      return $ TableRef table alias

join_kind = (reserved "inner" >> optional (reserved "join") >> return InnerJoin)
        <|> (reserved "outer" >> optional (reserved "join") >> return OuterJoin)
        <|> (reserved "cross" >> optional (reserved "join") >> return CrossJoin)

join_expr =
   do
      kind <- join_kind
      tr   <- table_ref
      reserved "on"
      cond <- bool_expr
      return $ JoinExpr kind tr cond

from_clause =
   do
      reserved "from"
      tr    <- table_ref
      joins <- many join_expr
      return $ FromClause tr joins

where_clause =
   do
      reserved "where"
      pred  <- bool_expr
      return $ WhereClause pred

query =
   do
      fc <- from_clause
      wc <- optionMaybe where_clause
      return $ Query fc wc

whileParser :: Parser Query
whileParser = white_space >> query

parseString :: String -> Query
parseString str =
  case parse whileParser "" str of
    Left  e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Query
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left  e -> print e >> fail "parse error"
       Right r -> return r
