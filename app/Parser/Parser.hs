module Parser.Parser (tokenize, lexicalMap) where
import qualified Types.Types as Types
import qualified Types.Lex as Lex
import qualified Types.Tokens as Tokens
import qualified Parser.Syntax as Syntax
import qualified Utils as Utils
-- import qualified 
lexicalMap :: [Char] -> [Types.Character]
lexicalMap (x:xs) | (x == Lex.space || x == Lex.tab || x == Lex.linefeed) = Lex.charToToken x:lexicalMap xs
			 	  | otherwise = lexicalMap xs
lexicalMap [] = []

tokenize :: [Types.Character] -> [Tokens.Token]
tokenize (x:xs) | (x == Types.SpaceChar) = Tokens.IMP [Types.SpaceChar]:rest where rest = stack xs
tokenize _ = []




stack (x:y:xs) | (x == Types.LineFeedChar && y == Types.SpaceChar) = Tokens.Operator [x, y]:(tokenize xs)
			   | (x == Types.TabChar && y == Types.SpaceChar) = Tokens.Operator [x, y]:(pushSign xs)
			   | (x == Types.LineFeedChar && y == Types.TabChar) = Tokens.Operator [x, y]:(tokenize xs)
			   | (x == Types.LineFeedChar && y == Types.LineFeedChar) = Tokens.Operator [x, y]:(tokenize xs)
			   | (x == Types.TabChar && y == Types.LineFeedChar) = Tokens.Operator [x, y]:(pushSign xs)
-- duplicate the top of the stack
stack (x:xs) | (x == Types.SpaceChar) = Tokens.Operator [x]:pushSign xs
-- push number on stack
stack _ = []

pushSign (x:xs)
	| (x == Types.SpaceChar || x == Types.TabChar) = (Tokens.Parameter (x:n)):(tokenize r)
	| otherwise = []
	where
		r = Utils.safeTail $ dropWhile notLineFeed xs
		n = pushNumber xs
pushSign _ = []

-- push number can return the rest of the symbol sequence that is yet to be parsed
-- pushNumber (x:xs) | (x == Types.SpaceChar || x == Types.TabChar) = x:pushNumber xs
-- 				  | (x == Types.LineFeedChar) = []
pushNumber list = takeWhile notLineFeed list

notLineFeed = (/= Types.LineFeedChar)

-- define tokenize in terms of a state machine
-- state transitions are a tuple of (currentState)

-- Start State :
-- if 1st input symbol is space -> go to stack state


-- Stack State : 

-- data States = Start | Stack

-- lexicalMap each token into groups that can represent elements of the abstract syntax tree
-- for stack case:
-- 



-- 'c''a''s''e' -> case keyword
-- '1'2'3' -> 123 number
-- ...

-- parse tokens = foldr (\token program -> program)



-- input = list of characters
-- from input create a parse tree using the grammar

-- to define parse tree, 
-- makeParseTree :: [Token.Character] -> Program.Program
-- makeParseTree x = _makeParseTree Program.EmptyProgram x

-- _makeParseTree :: Program.Program -> [Token.Character] -> Program.Program
-- _makeParseTree p [] = p
-- _makeParseTree p (x:xs) = _makeParseTree q xs where
-- 							q = case x of
-- 									Token.SpaceChar -> Program.EmptyProgram
-- 									Token.TabChar -> Program.EmptyProgram
-- 									Token.LineFeedChar -> Program.EmptyProgram