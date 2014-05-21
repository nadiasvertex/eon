module Compute where

data Op = Nop
		| Add
		| Sub
		| Mul
		| Div
		| Eq
		| Ne
		| Gt
		| Lt
		| Ge
		| Le
		deriving (Show)

data ColumnRefInfo = ColumnRefInfo {
	table	:: String,
	column  :: String
}

data ComputeVal = ColumnRef ColumnRefInfo
             | List [ComputeVal]
             | Number Integer
             | String String
             | Bool Bool

data BinOpNode = BinOpNode {
	binop :: Op,
	left  :: Node,
	right :: Node
}

data UnOpNode = UnOpNode {
	unop  :: Op,
	child :: Node
}

data Node = NodeError
		  | Leaf ComputeVal
		  | BinOp BinOpNode
		  | UnOp UnOpNode

showComputeVal :: ComputeVal -> String
showComputeVal (String contents) = "'" ++ contents ++ "'"
showComputeVal (Number contents) = show contents
showComputeVal (Bool contents) = show contents
showComputeVal (ColumnRef (ColumnRefInfo table column)) = table ++ "." ++ column

showNode :: Node -> String
showNode (Leaf contents) = show contents
showNode (BinOp (BinOpNode op left right)) = (show left) ++ (show op) ++ (show right)
showNode (UnOp (UnOpNode op child)) = (show op) ++ (show child)

instance Show ComputeVal where show = showComputeVal
instance Show Node where show = showNode