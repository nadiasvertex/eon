{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}

module Compute where

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data BoolExpr   = BoolConst Bool
                | Not BoolExpr
                | BoolBinary BoolBinOp BoolExpr BoolExpr
                | RelBinary RelBinOp ArithExpr ArithExpr
		          deriving (Show, Typeable, Generic)

data BoolBinOp  = And
                | Or
                deriving (Show, Typeable, Generic)

data RelBinOp   = GreaterThan
                | LessThan
                | GreaterOrEqual
                | LessOrEqual
                | JustEqual
                | NotEqual
                deriving (Show, Typeable, Generic)

data ArithExpr  = ColumnRef String String
                | IntConst Integer
                | Neg ArithExpr
                | ArithBinary ArithBinOp ArithExpr ArithExpr
                deriving (Show, Typeable, Generic)

data ArithBinOp = Add
                | Subtract
                | Multiply
                | Divide
                deriving (Show, Typeable, Generic)

data TableRef   = TableRef {
   table :: String,
   alias :: Maybe String
} deriving (Show, Typeable, Generic)

data JoinExpr   = JoinExpr JoinKind TableRef BoolExpr deriving(Show, Typeable, Generic)
data FromClause = FromClause TableRef [JoinExpr] deriving(Show, Typeable, Generic)

data JoinKind   = InnerJoin
                | OuterJoin
                | NaturalJoin
                | CrossJoin
                deriving (Show, Typeable, Generic)

data WhereClause = WhereClause BoolExpr deriving(Show, Typeable, Generic)

data Query = Query {
   from  :: FromClause,
   pred  :: Maybe WhereClause
} deriving (Show, Typeable, Generic)


instance Binary BoolExpr
instance Binary BoolBinOp
instance Binary RelBinOp
instance Binary ArithExpr
instance Binary ArithBinOp
instance Binary TableRef
instance Binary JoinExpr
instance Binary FromClause
instance Binary JoinKind
instance Binary WhereClause
instance Binary Query

