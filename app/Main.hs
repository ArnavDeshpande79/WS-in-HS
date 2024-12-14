module Main where
import System.IO
import System.Directory
-- import Parser.Syntax
import Types.SomeModule
-- import qualified Parser.Parser as Parser
import Text.Parsec.String (Parser)
import Text.Parsec.Prim

-- main :: IO ()
-- main = do
-- 	input <- readFile "app/sampleData/whitespace.txt"
-- 	let chars = (Parser.parse . Parser.tokenize . Parser.lexicalMap) input
-- 	putStrLn . show $ chars
-- 	return ()
main = return ""
-- todo:
-- 1. use the parser monad to parse
-- 2. make parse