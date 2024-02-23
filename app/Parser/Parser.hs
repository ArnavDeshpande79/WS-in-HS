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
-- tokenize (x:y:xs) = if (x == Types.TabChar && y == Types.SpaceChar) then (Tokens.IMP [x, y]):rest where rest = arithmetic xs
-- 					else if (x == Types.TabChar && y == Types.TabChar) then (Tokens.IMP [x, y]):rest where rest = heap xs
-- tokenize (x:y:xs) = case [x, y] of
-- 						[Types.TabChar, Types.SpaceChar] -> (Tokens.IMP [x, y]):rest where rest = arithmetic xs
-- 						[Types.TabChar, Types.TabChar] -> (Tokens.IMP [x, y]):rest where rest = heap xs
-- 						[Types.TabChar, Types.LineFeedChar] -> (Tokens.IMP [x, y]):rest where rest = io xs
-- tokenize (x:xs) = case x of
-- 					Types.SpaceChar -> (Tokens.IMP [x]):rest where rest = stack xs
-- 					Types.LineFeedChar -> (Tokens.IMP [x]):rest where rest = flow xs
-- 					_ -> []
-- tokenize _ = []
tokenize list = case list of
					Types.TabChar:Types.SpaceChar:xs -> (Tokens.IMP [Types.TabChar,Types.SpaceChar]):rest where rest = arithmetic xs
					Types.TabChar:Types.TabChar:xs -> (Tokens.IMP [Types.TabChar,Types.TabChar]):rest where rest = heap xs
					Types.TabChar:Types.LineFeedChar:xs -> (Tokens.IMP [Types.TabChar,Types.LineFeedChar]):rest where rest = io xs
					Types.SpaceChar:_:xs -> (Tokens.IMP [Types.SpaceChar]):rest where rest = stack xs
					Types.LineFeedChar:_:xs -> (Tokens.IMP [Types.LineFeedChar]):rest where rest = flow xs
					_ -> []
io (x:y:xs) = case [x, y] of
				[Types.SpaceChar, Types.SpaceChar] -> (Tokens.IMP [x]):rest where rest = tokenize xs
				[Types.SpaceChar, Types.TabChar] -> (Tokens.IMP [x]):rest where rest = tokenize xs
				[Types.TabChar, Types.SpaceChar] -> (Tokens.IMP [x]):rest where rest = tokenize xs
				[Types.TabChar, Types.TabChar] -> (Tokens.IMP [x]):rest where rest = tokenize xs
io _ = []

flow (x:y:xs) = case [x, y] of
					[Types.SpaceChar, Types.SpaceChar] -> (Tokens.Operator [x, y]):(pushSign xs)
					[Types.SpaceChar, Types.TabChar] -> (Tokens.Operator [x, y]):(pushSign xs)
					[Types.SpaceChar, Types.LineFeedChar] -> (Tokens.Operator [x, y]):(pushSign xs)
					[Types.TabChar, Types.SpaceChar] -> (Tokens.Operator [x, y]):(pushSign xs)
					[Types.TabChar, Types.TabChar] -> (Tokens.Operator [x, y]):(pushSign xs)
					[Types.TabChar, Types.LineFeedChar] -> (Tokens.Operator [x, y]):rest where rest = tokenize xs
					[Types.LineFeedChar, Types.LineFeedChar] -> (Tokens.Operator [x, y]):rest where rest = tokenize xs
					_ -> []
flow _ = []

-- label list = takeWhile notLineFeed list = pushNumber list

heap (x:xs) = case x of
				Types.SpaceChar -> r
				Types.TabChar -> r
			  where
				r = (Tokens.IMP [x]):rest where rest = heap xs
heap _ = []

arithmetic (x:y:xs) | (x == Types.SpaceChar && y == Types.SpaceChar) = Tokens.Operator [x, y]:rest
				    | (x == Types.SpaceChar && y == Types.TabChar) = Tokens.Operator [x, y]:rest
				    | (x == Types.SpaceChar && y == Types.LineFeedChar) = Tokens.Operator [x, y]:rest
				    | (x == Types.TabChar && y == Types.SpaceChar) = Tokens.Operator [x, y]:rest
					| (x == Types.TabChar && y == Types.TabChar) = Tokens.Operator [x, y]:rest
						where
							rest = tokenize xs
arithmetic (x:xs) | (x == Types.SpaceChar) = Tokens.Operator [x]:rest
				  | (x == Types.TabChar) = Tokens.Operator [x]:rest
					where
						rest = tokenize xs
arithmetic _ = []


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