module Tokenizer where

import Data.List as List
import Data.Maybe as Maybe
import qualified Data.Map as Map hiding (map)

-- most naive implementation possible

--this lexer always fails
--main function
--lexer :: IOFile -> Tokenlist
--lexer = \file -> Nothing

data Token = 
	  Number Int 
	| Id String
	| Boolean Bool
	| Operator
	| Keyword
	| Seperato
	| WhiteSpace
	deriving (Show, Eq)

type Reader = (String,Int) 					--leeskop
type Tokenlist = Maybe [Token]				--lijst van tokens of niets
type Lexfun = Reader -> (Reader,Tokenlist)	--stdtype van input naar (resultaat,onbewerkte_input)

keywordmap = Map.fromList
	[	("if",Keyword),
		("then",Keyword),
		("else",Keyword),
		("while",Keyword)
	]


after :: Lexfun -> Lexfun -> Lexfun
after f g = \x -> 
	case (f x) of 
		(_,Nothing) -> g x
		(r,Just list) -> (r,Just list)

lexStr :: Reader -> Tokenlist
lexStr ([],i) = Nothing
lexStr (xs,i) = case lexStr (newreader) of 
				Nothing -> Nothing
				Just tokens -> Just (tokens ++ fromMaybe [] (lexStr newreader))
	where (newreader,result) = lexOneToken (xs,i)

lexOneToken :: Lexfun
lexOneToken = \x -> (lexWhitespace `after` lexComment `after` lexKeyword) x


lexComment :: Lexfun
lexComment (input,index)
	| isPrefixOf "//" input 		= lexLineComment (fromJust (stripPrefix "//" input)) 
	| isPrefixOf "/*" input			= lexMultiComment (fromJust (stripPrefix "/*" input))
	| otherwise						= ((input,index),Nothing)
	where
		lexLineComment input = 	let	(comment,(x:rest)) = break (\x -> x == '\n') input
								in	((rest,index+(length comment)),Just [])
		lexMultiComment input = let (comment,rest) = splitAtEndComment [] input
								in ((rest,index+(length comment)),Just [])

splitAtEndComment :: String -> String -> (String,String)
splitAtEndComment c [] = (c,"")
splitAtEndComment [] _ = ("","")
splitAtEndComment c (x:xs) = if (x == '*' && (head xs) == '/') 
								then (c,tail xs) 
								else splitAtEndComment (c++[x]) xs

lexWhitespace :: Lexfun
lexWhitespace (input,index) =
	case input of 
		('\r':xs)	-> ((xs,index+1),empty)
		('\n':xs)	-> ((xs,index+1),empty)
		('\t':xs)	-> ((xs,index+1),empty)
		(' ':xs)	-> ((xs,index+1),empty)
		_			-> ((input,index),Nothing)
	where empty = Just []
		

lexKeyword :: Lexfun
lexKeyword (input,index) =
	let matches = splitOnKeyword input
	in 
		case matches of	
			[] -> ((input,index),Nothing)
			((keyword,rest):xs)	-> ((rest,index+(length keyword)), Just ([ fromJust (Map.lookup keyword keywordmap)]))

keywordlist = ["if","then","else","while","do","return"]

splitOnKeyword input = (Maybe.catMaybes (map (\keyword -> 
						if keyword `isPrefixOf` input 
							then Just (keyword, fromJust (stripPrefix keyword input)) 
							else Nothing) keywordlist))
