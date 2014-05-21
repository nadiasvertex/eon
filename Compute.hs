module Compute where

data BoolExpr   = BoolConst Bool
                | Not BoolExpr
                | BoolBinary BoolBinOp BoolExpr BoolExpr
                | RelBinary RelBinOp ArithExpr ArithExpr
                deriving (Show)

data BoolBinOp  = And
                | Or deriving (Show)

data RelBinOp   = GreaterThan
                | LessThan
                | GreaterOrEqual
                | LessOrEqual
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
