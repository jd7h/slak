module Parser where

import Tokens

data SL = Tmp

type AST = Maybe [Tree SL]
type Parsfun = [Token] -> ([Token],AST)

kleene :: Parsfun -> Parsfun					-- de "*" in de grammatica
(\/) :: Parsfun -> Parsfun -> Parsfun		-- representeert de "|" in de grammatica
(>>) :: Parsfun -> Parsfun -> Parsfun		-- zoals andthen
opt :: Parsfun -> Parsfun						-- representeerd de [ optionele argumenten ] in de gramm

parseGoal :: Parsfun -- Goal = (VarDecl | FunDecl)+
parseGoal input = (parseVarDecl \/ parseFunDecl) >> kleene (parseVarDecl \/ parseFunDecl)

parseVarDecl :: Parsfun
parseVarDecl = parseType >> parseId >> parseSep Eq >> parseExp >> parseSep PComma -- exp is hier de 'body'

parseFunDecl :: Parsfun
parseFunDecl = parseRetType >> parseId >> parseSep LPar >> opt parseArgs >> parseSep RPar >> parseSep LAcc >> parseBody >> parseSep RAcc --zoiets, waarbij body nog moet worden gedefinieerd

parseSep :: Token -> Parsfun
parseSep t [] = ([],Nothing)
parseSep (Sep s) (x:xs) = if t == x then (xs,Just []) else ((x:xs),Nothing) --(symbolen eten we op, right?) tenzij het een operator is)
parseSep t tl = (tl,Nothing) --voor al de input geen separator is






