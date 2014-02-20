module Tokenizer where

import Data.List as List
import Data.Maybe as Maybe
import qualified Data.Map as Map hiding (map)
import qualified Data.Char as C

-- most naive implementation possible

--this lexer always fails
--main function
--lexer :: IOFile -> Tokenlist
--lexer = \file -> Nothing

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
--	| Neg
	| Not
	| GrEq
	| SmEq
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
		

type Reader = (String,Int) 					--leeskop
type Tokenlist = Maybe [Token]				--lijst van tokens of niets
type Lexfun = Reader -> (Reader,Tokenlist)	--stdtype van input naar (resultaat,onbewerkte_input)

--reservedSymbols = []

reservedSymbols =
	[	--operators
		("*",Op Times),
		("/",Op Div),
		("+",Op Plus),
		("-",Op Minus),
		("!",Op Not),
		(">=",Op GrEq),
		("<=",Op SmEq),
		("==",Op Eq),
		("=",Op As),
		("&&",Op And),
		("||",Op Or),
		(":",Op Concat),
		--separators
		("[",Sep LBr),
		("]",Sep RBr),
		("{",Sep LAcc),
		("}",Sep RAcc),
		("(",Sep LPar),
		(")",Sep RPar),
		(",",Sep Comma),
		(";",Sep Pcomma)	
	]


reservedWords = 
	[	("if",Key If),
		("then",Key Then),
		("else",Key Else),
		("while",Key While),
		("Int",T INT),
		("Void",T VOID),
		("Bool",T BOOL),
		("False",Boolean False),
		("True",Boolean True),
		("return",Key Return)
	]


{-
keywordmap = Map.fromList
	[	("if",Keyword),
		("then",Keyword),
		("else",Keyword),
		("while",Keyword)
	]

keywordlist =
	[	("if",Keyword),
		("then",Keyword),
		("else",Keyword),
		("while",Keyword)
	]
-}

--combinator for lexer functions
andthen :: Lexfun -> Lexfun -> Lexfun
andthen f g = \x -> 
	case (f x) of 
		(_,Nothing) -> g x
		(r,Just list) -> (r,Just list)

--lexes the whole program string
lexStr :: Reader -> Tokenlist
lexStr ([],i) = Nothing
lexStr (xs,i) = if success then Just (fromJust result ++ (fromMaybe [] (lexStr (rinput,rindex)))) else Nothing
	where 
	((rinput,rindex),result) = lexOneToken (xs,i)
	success = isJust result

{-
--lexes one whole program string
lexStr :: Reader -> Tokenlist
lexStr ([],i) = Nothing
lexStr (xs,i) = case lexStr (newreader) of 
				Nothing -> Nothing
				Just tokens -> Just (tokens ++ fromMaybe [] (lexStr newreader))
	where (newreader,result) = lexOneToken (xs,i)
-}

--lexes one token, at the moment chooses FIRST match, must go to LONGEST match
lexOneToken :: Lexfun
lexOneToken = \(input,index) -> (lexWhitespace `andthen` lexComment `andthen` lexInteger `andthen` lexSymbol `andthen` lexKeyword) (input,index)

lexComment :: Lexfun
lexComment (input,index)
	| isPrefixOf "//" input			= lexLineComment (drop 2 input)
	| isPrefixOf "/*" input			= lexMultiComment (drop 2 input)
	| otherwise						= ((input,index),Nothing)
	where
		lexLineComment input = 	let	(comment,(x:rest)) = break (\x -> x == '\n' || x == '\r') input
								in	((rest,index+(length comment)),Just [])
		lexMultiComment input =  if "*/" `isInfixOf` input 
								 then 
									let (comment,rest) = splitAtEndComment [] input
									in ((rest,index+length comment),Just [])
								 else (("",index+length input),Nothing)


splitAtEndComment :: String -> String -> (String,String)
splitAtEndComment acc []			= (acc,[])
splitAtEndComment acc ('*':'/':xs)  = (acc,xs)
splitAtEndComment acc (x:xs) 		= splitAtEndComment (acc++[x]) xs

{-
--lexes comments
lexComment :: Lexfun
lexComment (input,index)
	| isPrefixOf "//" input 		= lexLineComment (fromJust (stripPrefix "//" input)) 
	| isPrefixOf "/*" input			= lexMultiComment (fromJust (stripPrefix "/*" input))
	| otherwise						= ((input,index),Nothing)
	where
		lexLineComment input = 	let	(comment,(x:rest)) = break (\x -> x == '\n' || x == "\r") input
								in	((rest,index+(length comment)),Just [])
		lexMultiComment input = let (comment,rest) = splitAtEndComment [] input
								in ((rest,index+(length comment)),Just [])

--splits a string at a comment delimiter
splitAtEndComment :: String -> String -> (String,String)
splitAtEndComment c [] = (c,"")
splitAtEndComment [] _ = ("","")
splitAtEndComment c (x:xs) = if (x == '*' && (head xs) == '/') 
								then (c,tail xs) 
								else splitAtEndComment (c++[x]) xs
-}

--lexes (and discards) whitespace
lexWhitespace :: Lexfun
lexWhitespace (input,index) =
	case input of 
		('\r':xs)	-> ((xs,index+1),empty)
		('\n':xs)	-> ((xs,index+1),empty)
		('\t':xs)	-> ((xs,index+1),empty)
		(' ':xs)	-> ((xs,index+1),empty)
		_			-> ((input,index),Nothing)
	where empty = Just []

--lexes integers
lexInteger :: Lexfun
lexInteger (input,index)
	| C.isDigit (head input)	=	let (integer,rest) = splitAtInt [] input
									in ((rest,index+(length integer)),Just [(Number (read (integer) :: Int))])
	| otherwise					= ((input,index),Nothing)
	where
	splitAtInt integer [] = (integer,"")
	splitAtInt integer (x:xs) = if C.isDigit x then splitAtInt (integer++[x]) xs else (integer,(x:xs))

--lexes special symbols such as operators
lexSymbol :: Lexfun
lexSymbol (input,index)
	| C.isAlphaNum (head input)	= ((input,index),Nothing)
	| otherwise						=	let 
										(sym,alphanum) = span isWeirdSymbol input
										match = longestMatch sym
										rest = if isJust match then drop (length (fst (fromJust match))) input else input
										nindex = if isJust match then index + length (fst (fromJust match)) else index
										tok = if isJust match then Just [snd (fromJust match)] else Nothing
										in ((rest,nindex),tok)
		where
			longestMatch []				= Nothing
			longestMatch symbols@(x:xs) = if isJust (lookup symbols reservedSymbols) then Just (symbols, fromJust (lookup symbols reservedSymbols)) else longestMatch xs
			isWeirdSymbol s = C.isMark s || C.isSymbol s || C.isPunctuation s

--lexes special symbols such as operators
{-
lexSymbol :: Lexfun
lexSymbol (input,index)
	| not (C.isAlphaNum (head input))	=	let 
											(allSymbols,alphanumrest) = (span (not C.isAlphaNum) input)
											maybeSymTok = longestMatch allSymbols
											rest = if isJust maybeSymbol then fromJust (stripPrefix symbol input) else input
											in if isJust maybeSymbol then ((rest,index+(length symbol)),Just [(assignSymbolToken symbol)])
	| otherwise					= ((input,index),Nothing)
	where 
		isSpecial s = isJust (lookup s reservedSymbols)
		assignSymbolToken s = fromJust (lookup s reservedSymbols)
		longestMatch []				= Nothing
		longestMatch symbols@(x:xs) = if isJust (lookup symbols reservedSymbols) then Just (symbols, else longestMatch xs
-}

--lexes keywords and variable names
lexKeyword :: Lexfun
lexKeyword ([],index) = (([],index),Nothing)
lexKeyword ((x:xs),index)
	| C.isAlpha x	=	let (identifier,rest) = (x : (takeWhile C.isAlphaNum xs),dropWhile C.isAlphaNum xs)
						in ((rest,index+(length identifier)),Just [assignToken (identifier)])
	| otherwise		=	(((x:xs),index),Nothing)
	where
		assignToken str = if isJust (lookup str reservedWords) then fromJust (lookup str reservedWords) else Id str
