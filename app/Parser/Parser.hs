module Parser.Parser (makeParseTree) where
import qualified Types.Tokens as Token
import qualified Types.Program as Program
-- to define parse tree, 
makeParseTree :: [Token.Character] -> Program.Program
makeParseTree x = _makeParseTree Program.EmptyProgram x

_makeParseTree :: Program.Program -> [Token.Character] -> Program.Program
_makeParseTree p [] = p
_makeParseTree p (x:xs) = _makeParseTree q xs where
							q = case x of
									Token.SpaceChar -> Program.EmptyProgram
									Token.TabChar -> Program.EmptyProgram
									Token.LineFeedChar -> Program.EmptyProgram