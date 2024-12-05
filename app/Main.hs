module Main where
import System.IO
import System.Directory
import qualified Parser.Parser as Parser

main :: IO ()
main = do
	input <- readFile "app/sampleData/whitespace.txt"
	let chars = (Parser.parse . Parser.tokenize . Parser.lexicalMap) input
	putStrLn . show $ chars
	return ()

-- todo:
-- 1. use the parser monad to parse
-- 2. make parse