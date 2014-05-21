module Compute where

data BoolExpr   = BoolConst Bool
                | Not BoolExpr
                | BoolBinary BoolBinOp BoolExpr BoolExpr
                | RelBinary RelBinOp ArithExpr ArithExpr
		          deriving (Show)

data BoolBinOp  = And
                | Or
                deriving (Show)

data RelBinOp   = GreaterThan
                | LessThan
                | GreaterOrEqual
                | LessOrEqual
                | JustEqual
                | NotEqual
                deriving (Show)

data ArithExpr  = ColumnRef String String
                | IntConst Integer
                | Neg ArithExpr
                | ArithBinary ArithBinOp ArithExpr ArithExpr
                deriving (Show)

data ArithBinOp = Add
                | Subtract
                | Multiply
                | Divide
                deriving (Show)

data TableRef   = TableRef {
   table :: String,
   alias :: Maybe String
} deriving (Show)

data JoinExpr   = JoinExpr JoinKind TableRef BoolExpr deriving(Show)
data FromClause = FromClause TableRef [JoinExpr] deriving(Show)

data JoinKind   = InnerJoin
                | OuterJoin
                | NaturalJoin
                | CrossJoin
                deriving (Show)

data WhereClause = WhereClause BoolExpr deriving(Show)

data Query = Query {
   from  :: FromClause,
   pred  :: Maybe WhereClause
} deriving (Show)

