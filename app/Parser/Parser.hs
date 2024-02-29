module Parser.Parser (tokenize, lexicalMap, parse, toNumber) where
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
tokenize list = case list of
					-- Types.TabChar:Types.SpaceChar:xs -> (Tokens.IMP [Types.TabChar,Types.SpaceChar]):rest where rest = arithmetic xs
					-- Types.TabChar:Types.TabChar:xs -> (Tokens.IMP [Types.TabChar,Types.TabChar]):rest where rest = heap xs
					-- Types.TabChar:Types.LineFeedChar:xs -> (Tokens.IMP [Types.TabChar,Types.LineFeedChar]):rest where rest = io xs
					Types.SpaceChar:xs -> (Tokens.IMP [Types.SpaceChar]):rest where rest = stack xs
					-- Types.LineFeedChar:_:xs -> (Tokens.IMP [Types.LineFeedChar]):rest where rest = flow xs
					_ -> []

parse :: [Tokens.Token] -> Syntax.Program
parse tokens = case tokens of
					-- IMP list -> case list of
					-- 				[Types.SpaceChar] -> Syntax.StackInstr ()
					-- 				-- [Types.TabChar:Types.SpaceChar] ->
					-- 				-- [Types.TabChar:Types.TabChar] ->
					-- 				-- [Types.LineFeedChar] ->
					-- 				-- [Types.TabChar:Types.LineFeedChar] ->
					-- 				_ -> []
					-- Operator list
					-- Parameter list
					(Tokens.IMP i):(Tokens.Operator o):Tokens.Parameter p:rest -> case (i, o) of
																				([Types.SpaceChar], [Types.SpaceChar]) -> (Syntax.StackInstr (Syntax.StackPush (toNumberWithSign p))):parse rest
																				-- _ -> 
					-- Tokens.IMP i:Tokens.Operator o:rest -> 
					_ -> []

io (x:y:xs) = case [x, y] of
				[Types.SpaceChar, Types.SpaceChar] -> (Tokens.IMP [x]):r
				[Types.SpaceChar, Types.TabChar] -> (Tokens.IMP [x]):r
				[Types.TabChar, Types.SpaceChar] -> (Tokens.IMP [x]):r
				[Types.TabChar, Types.TabChar] -> (Tokens.IMP [x]):r
			  where
				r = tokenize xs
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

toNumber :: [Types.Character] -> Int
toNumber n = sum (zipWith (\x y -> (case x of
									Types.SpaceChar -> 0
									Types.TabChar -> 1
									) * (2^y)
					) n (reverse [0..(length n - 1)]))
toNumber _ = 0


toNumberWithSign :: [Types.Character] -> Int
toNumberWithSign (s:s') = sign * toNumber s' where sign = if (s == Types.SpaceChar) then 1 else (-1)