module Main where
import System.IO
import System.Directory
import qualified Parser.Parser as Parser
import qualified Types.Types as Types
import qualified Types.Tokens as Tokens

main :: IO ()
main = do
	input <- readFile "app/sampleData/whitespace.txt"
	let chars = (Parser.parse . Parser.tokenize . Parser.lexicalMap) input
	putStrLn . show $ chars
	return ()
    -- let parseTree = makeParseTree tokens
-- grammar:
-- IMP = Instruction Modification Parameter
-- data Program = IMP Program | Empty
-- S -> (IMP S) | E
-- IMP -> Space Stack
-- 			| Tab Space Arithmetic
-- 			| Tab Tab SpaceTab -- heap
-- 			| LF Flow
-- 			| Tab LF InputOutput
-- Stack -> Space Parameter | LF Space | Tab Space Parameter | LF Tab | LF LF | Tab LF Parameter
-- Parameter -> SpaceTab Number LF
-- SpaceTab -> Space | Tab
-- Number -> SpaceTab Number | SpaceTab

-- Arithmetic -> Space Space | Space Tab | Space LF | Tab Space | Tab Tab | Space | Tab
-- Flow -> Space Space Label | Space Tab Label | Space LF Label | Tab Space Label | Tab Tab Label | Tab LF | LF LF
-- InputOutput -> Space Space | Space Tab | Tab Space | Tab Tab
-- Label -> LabelRepeat LF
-- LabelRepeat -> SpaceTab LabelRepeat | SpaceTab




-- -- whitespace contains only three characters
-- -- space ' '
-- -- tab '\t'
-- -- linefeed '\n'
-- -- step 1 : given a whitespace text -> pass it through a lexical analyser
-- -- input : a program text
-- output : a parse tree
