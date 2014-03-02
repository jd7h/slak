module AST where

type Program = [ Declaration ]
data Declaration = V VarDecl | F FunDecl deriving (Show,Eq)
type VarDecl = Type Id Value
type FunDecl = Type Id [Arg] Sequence

type Sequence = ( [ VarDecl ],[ Body ])
data Body = Var v | Stm s deriving (Show,Eq)

