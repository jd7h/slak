module AST where

-- Below largely follows overall structure
-- I've used datatypes rather than standard types to allow for flexibility in adjustments
data Program 		= Program [Declaration]

data Declaration	= VarDecl Type Identifier Expression
					| FunDecl Type Identifier [(Type, Identifier)] [Declaration] [Statement]

data Type			= Void
					| Number
					| Boolean

data Statement		= Expression Expression					-- To support function calls
					| Assignment Identifier [Field] Expression
					| If Expression [Statement] [Statement]
					| While Expression [Statement]
					| Return (Maybe Expression)

data Expression		= Var Identifier [Field]
					| BinOp Expression Op2 Expression		-- Enables tree simplification
					| UnOp Op1 Expression			
					| Constant Type 
					| FunCall Identifier [Expression]
					| Pair Expression Expression
					| Nil
					
data Field			= Head | Tail | First | Second
					
data Op2			= Plus | Min | Mult | Div | Mod
					| Eq | Lt | Gt | Le | Ge | Neq
					| And | Or
					| Cons

data Op1			= Neg | Not

data Identifier		= Identifier String Int