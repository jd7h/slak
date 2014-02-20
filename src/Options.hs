module Options where

import System.Console.GetOpt

data Options = Options
	{ verbose	:: Bool
	, snail		:: Bool
	} deriving Show

defaultOptions :: Options
defaultOptions = Options
	{ verbose	= False
	, snail	= False
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['v']	["verbose"]	(NoArg (\ opts -> opts { verbose = True }))	"Additional descriptive text to StdIO"
	, Option ['s']	["snail"]	(NoArg (\ opts -> opts { verbose = True }))	"SNAIL"
	]

parseArguments :: [String] -> IO (Options, [String])
parseArguments argv =
	case getOpt Permute options argv of
		(o, n, [] ) -> return (foldl (flip id) defaultOptions o, n)
		(_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "Usage: ic [OPTION...] files..."