module Main where
import Types
import System.IO
import System.Directory

main :: IO (Int)
main = do
	input <- readFile "./data/whitespace.txt"
	let tokens = filter (/=Types.Invalid) . (map Types.parse) $ input
	--let parseTree = makeParseTree tokens
	in putStrLn (show tokens)
	return 0