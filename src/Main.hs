import System.Environment
import System.Exit

import Control.Monad

import Options
import Tokenizer

-- type CompFun = Options -> String -> String -> [Token] -> IO (...)

main = do
	(options, [file]) <- getArgs >>= parseArguments
	-- TODO: Clearly define console interface functions rather than using putStrLn. And snail. Definitely snail.
	when (snail options) $ putStrLn ("~~ ADD SNAIL HERE ~~")
	when (verbose options) $ putStrLn ("* Processing " ++ file ++ " *")

	source <- readFile file
	when (verbose options) $ putStrLn ("* Done reading file *")

	(tokenResult, tokenErrors) <- tokenize options file source (Just [])

	exitSuccess
		
	
-- tokenize :: CompFun
tokenize options file source _ = do
	let tokenResult = lexStr (source, 0)
	return $ case tokenResult of
		Nothing -> ([], [])
		Just x -> (x, [])
		
