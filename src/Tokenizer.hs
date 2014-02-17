module Tokenizer where

import Data.List as List
import Data.Maybe as Maybe
import Data.Map as Map hiding (map)

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
type Tokenlist = Maybe [Token]					--lijst van tokens of niets
type Lexfun = Reader -> (Reader,Tokenlist)	--stdtype van input naar (resultaat,onbewerkte_input)

keywordmap = fromList
	[	("if",Keyword),
		("then",Keyword),
		("else",Keyword),
		("while",Keyword)
	]

{-
readOneToken :: Lexfun
readOneToken (input,index) =
	let (newreader,tokenlist) = break isTokenstring input --kan niet want break :: (string -> bool) -> string -> (string,string)
	in case tokenlist of
-}

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
