module Tokens where

data Keyword =
	  If
	| Then
	| Else
	| While
	| Return
	deriving (Show, Eq)

data Token = 
	  Number Int 
	| Id String
	| Boolean Bool
	| Op Operator
	| Key Keyword
	| T Type
	| Sep Separator
	deriving (Show, Eq)

data Operator = 
	  Plus
	| Minus
	| Times
	| Div
	| Not
	| GrEq
	| SmEq
	| Gr
	| Sm
	| Eq
	| As
	| And
	| Or
	| Concat
	deriving (Show,Eq)

data Separator =
	  LBr
	| RBr
	| LAcc
	| RAcc
	| LPar
	| RPar
	| Comma
	| Pcomma
	deriving (Show, Eq)

data Type = 
	  INT
	| VOID
	| BOOL	
	deriving (Show, Eq)

type Tokenlist = Maybe [Token]


