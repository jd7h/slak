module AST where

type Program 		= [ Declaration ]
data Declaration	= Var VarDecl
			| Fun FunDecl
type VarDecl		= Type Id Expression
type FunDecl		= Type Id [Arg] Sequence
type Arg		= Type Id
type Sequence		= ([VarDecl],[Statement])
data Statement		= Ass Id [Field] Expression			-- Variable assignment
			| If Expression [Statement] [Statement]		-- If
			| While Expression [Statement]			-- While
			| Call Funcall					-- Function call
			| Return Expression				-- Return
data Expression		= Access Id [Field]				-- Lookup	Lijst van 0 of meer field selectors

			-- DIT				-- Expressief, maar niet erg generiek. Typecheckker moet toch controleren of zo'n expressie klopt
							-- Kan je wel foutieve arithmetische operaties niet als syntaxfout markeren
			| BinOp Op2 Expression Expression
			| UnOp Op1 Expression
			-- OF WEGLATEN en als functieaanroep modelleren. 
			
			| Call FunCall
			| ... -- Work in Progress
	
data FunCall		= Fun Id [Expression]
			| Op Operator [Expression]	-- Deze regel komt er dan bij
			
data Op2		= Plus | Min | Times | Divide | Mod
			| Eq | Lt | Gt | Le | Ge | Neq
			| And | Or
			| Cons
data Op1		= Neg | Not

data Operators		= ...	-- Lijst met alle operatoren

data Field		= Head | Tail | First | Second
data			= Int | Bool
type Id			= String

