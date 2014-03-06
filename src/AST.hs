module AST where

-- Below largely follows overall structure
-- I've used datatypes rather than standard types to allow for flexibility in adjustments
data Program 		= Program [Declaration]
					deriving (Show, Eq, Read)

data Declaration	= VarDecl Type Identifier Expression
					| FunDecl Type Identifier [(Type, Identifier)] [Declaration] [Statement]
					deriving (Show, Eq, Read)

data Type			= Void
					| Number
					| Boolean
					deriving (Show, Eq, Read)

data Statement		= Expression Expression					-- To support function calls
					| Assignment Identifier [Field] Expression
					| If Expression [Statement] [Statement]
					| While Expression [Statement]
					| Return (Maybe Expression)
					deriving (Show, Eq, Read)

data Expression		= Var Identifier [Field]
					| BinOp Expression Op2 Expression		-- Enables tree simplification
					| UnOp Op1 Expression			
					| Constant Type 
					| FunCall Identifier [Expression]
					| Pair Expression Expression
					| Nil
					deriving (Show, Eq, Read)
					
data Field			= Head | Tail | First | Second
					deriving (Show, Eq, Read)
					
data Op2			= Plus | Min | Mult | Div | Mod
					| Eq | Lt | Gt | Le | Ge | Neq
					| And | Or
					| Cons
					deriving (Show, Eq, Ord, Read)

data Op1			= Neg | Not
					deriving (Show, Eq, Ord, Read)

data Identifier		= Identifier String Int
					deriving (Show, Eq, Read)