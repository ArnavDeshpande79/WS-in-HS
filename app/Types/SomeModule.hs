module Types.SomeModule where
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Syntax as Syntax
import Constants

[spaceChar, tabChar, linefeedChar] = map char "stn" :: [Parser Char]
custom a b = (a + b) * (a + b + 1)
custom' i j = 1 + (custom i j)
-- why is this allowed
f f' x = case f' of
			(*) -> f' (f' x x) (f' x x)
			(+) -> f' x x
			custom' -> -1
			-- (\a b -> (a + b) * (a + b + 1)) -> 0
			custom -> f' (custom x x) (custom x x)

-- write custom case before (+) and the result changes
-- f (\i j -> (i-1) + (j-2) + 3) 10 = 20
-- f (\i j -> i + j) 10 = 20
-- f (\i j -> (pred i) + (succ j)) 10
-- f ((\i j o -> i + j + o) $ 0) 10

-- write (*) before (+) and the result changes
-- (f (*) 10, f (+) 10) = (10000,40)
-- (f (*) 10, f (+) 10) = (100,20)

-- also,
-- f f' x = case f' of
-- 			(+) -> f' x x
-- 			(*) -> f' (f' x x) (f' x x)
-- 			(\a b -> (a + b) * (a + b + 1)) -> 0
-- 			custom -> f' (custom x x) (custom x x)

-- Error
-- Lambda-syntax in pattern.
-- Pattern matching on functions is not possible.
-- |
-- 13 |                         (\a b -> (a + b) * (a + b + 1)) -> 0


-- but not this:
-- f f' x
-- 	= if f' == (+) then
-- 		f' x x
-- 	  else if f' == (*) then
-- 		f' x x
-- 	  else
-- 		f' (x + 1) x
-- allowing function comparing in case expressions ??


wsNumber = many1 (char 's' <|> char 't') :: Parser String

endOfInput = string "nnn" :: Parser String

toN ('s':s') = _toN s'
toN (_:s') = - (_toN s')

_toN n = sum (zipWith (\x y -> (case x of
									's' -> 0
									't' -> 1
									) * (2^y)
					) n (reverse [0..(length n - 1)]))

program :: Parser Syntax.Program
program = many1 statement

imp :: Parser String
imp = (try (string "tn")) <|> (try (string "ts")) <|> (try (string "tt")) <|> (try (string "n")) <|> (try (string "s"))

opIO :: Parser String
opIO = (try (string "ss")) <|> (try (string "st")) <|> (try (string "ts")) <|> (try (string "tt"))

opHeap :: Parser String
opHeap = (try (string "s")) <|> (try (string "t"))

opArithmetic :: Parser String
opArithmetic = (try (string "ss")) <|> (try (string "st")) <|> (try (string "sn")) <|> (try (string "ts")) <|> (try (string "tt"))

opFlow :: Parser String
opFlow = (try (string "ss")) <|> (try (string "st")) <|> (try (string "sn")) <|> (try (string "ts")) <|> (try (string "tt")) <|> (try (string "tn")) <|> (try (string "nn"))

opStack :: Parser String
opStack = (try (string "ns")) <|> (try (string "ts")) <|> (try (string "nt")) <|> (try (string "nn")) <|> (try (string "tn")) <|> (try (string "s"))

io :: String -> Syntax.Code
io "ss" = Syntax.OutputChar
io "st" = Syntax.OutputInt
io "ts" = Syntax.ReadCharacter
io "tt" = Syntax.ReadInt

heap :: String -> Syntax.Code
heap "s" = Syntax.Store
heap "t" = Syntax.Retrieve

arithmetic :: String -> Syntax.Code
arithmetic "ss" = Syntax.Addition
arithmetic "st" = Syntax.Subtraction
arithmetic "sn" = Syntax.Multiplication
arithmetic "ts" = Syntax.IntegerDivision
arithmetic "tt" = Syntax.Modulo

flow :: String -> Syntax.Code
flow "ss" = Syntax.Mark
flow "st" = Syntax.Call
flow "sn" = Syntax.Jump
flow "ts" = Syntax.JumpIfZero
flow "tt" = Syntax.JumpIfNegative
flow "tn" = Syntax.SubRoutineEnd
flow "nn" = Syntax.ProgramEnd

stack :: String -> Syntax.Code
stack "ns" = Syntax.StackPush
stack "ts" = Syntax.StackDuplicate
stack "nt" = Syntax.StackCopy
stack "nn" = Syntax.StackSwap
stack "tn" = Syntax.StackDiscard
stack "s" = Syntax.StackSlide

-- statement :: Parser Syntax.Instruction
statement = do {
	list <- imp;
	case list of
		"tn" -> do {
			list' <- opIO;
			let
				op = io list'
			in
				return (op, [])
		}
		"ts" -> do {
			list' <- opHeap;
			let
				op = heap list'
			in
				return (op, [])
		}
		"tt" -> do {
			list' <- opArithmetic;
			let
				op = arithmetic list'
			in
				return (op, [])
		}
		"n" -> do {
			list' <- opFlow;
			let
				op = flow list'
			in
				case op of
					Syntax.SubRoutineEnd -> return (op, [])
					Syntax.ProgramEnd -> return (op, [])
					_ -> do {
						l <- wsNumber;
						return (op, [read l] :: [Int]);
					}
		}
		"s" -> do {
			list' <- opStack;
			let
				op = stack list'
			in
				case op of
					Syntax.StackPush -> return (op, [])
					Syntax.StackCopy -> return (op, [])
					Syntax.StackSlide -> return (op, [])
					_ -> do {
						l <- wsNumber;
						return (op, [read l] :: [Int]);
					}
		}
}