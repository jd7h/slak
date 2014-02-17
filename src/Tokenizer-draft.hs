module Tokenizer where

import Data.Char as C

data Token = 
	  Number Int 
	| Id String
	| Boolean Bool
	| Operator
	| Keyword --String if we do this, can we go from | processKeyword (x:xs) = Keyword x???com
	| Seperator --String
	| WhiteSpace
	deriving (Show, Eq)

data Keyword =
	  If
	| Then
	| Else
	| While
	deriving (Show, Eq)

data Operator = 
	  Plus
	| Minus
	| Times
	| Div
	| Neg
	| Not
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

Leeskop = (String,Int)
Tokenlist = Maybe [Token]

TokenFunction = Leeskop -> Tokenlist -> Leeskop -> Tokenlist

processWhiteSpace :: (String,Int) -> (String,Int)
processWhiteSpace (s@(x:xs),t)			= if C.isSpace x then processWhiteSpace (xs,t+1) else (s,t)

processNumber :: (String,Int) -> Maybe Token
processNumber (s@(x:xs),t)			= if C.isDigit x then tokAdd x (processNumber (xs,t+1)) else Nothing
	where
		tokAdd :: Char -> (Maybe Token) -> (Maybe Token)
		tokAdd s Nothing	= Just (Number (C.digitToInt s))
		tokAdd s (Just (Number i))		= Just (Number (read (s:(show i)) :: Int))
		tokAdd s _ = Nothing

--Volgorde:
-- whitespace -> comments -> brackets -> keywords -> numbers -> ids -> en weer terug naar het begin en de lijst van tokens opleveren!

--Types moeten goed op elkaar aansluiten = moeilijk!
--processEverything :: (String,Int) -> (Maybe [Token])
--processEverything ([],_) = Just []
--processEverything (s@(y:ys),i) = processEverything . processNumber . processWhiteSpace (s,i)
