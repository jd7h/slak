module AST where

type Program = [ Declaration ] -- a program is a list of declarations
data Declaration = V VarDecl | F FunDecl deriving (Show,Eq) -- a declaration is either a variable decl or a function decl
type VarDecl = Type Id Expression -- a variable declaration consists of a Type, an Id and an expression
type FunDecl = Type Id [Arg] Sequence -- a function declaration has a Type, an Id, a (possibly empty) list of arguments and a function body
type Sequence = ( [ VarDecl ],[ Body ]) -- a function body consists of first a list of (local variable declarations and then some statements)
data Body = Var v | Stm s deriving (Show,Eq)






