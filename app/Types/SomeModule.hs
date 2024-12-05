-- write the constants
module Types.SomeModule where
import Text.Parsec
import Text.Parsec.String (Parser)
import Constants

data Token = Space | LineFeed | Tab | Unknown deriving (Eq, Show)

toToken a | a == spaceChar = Space
		  | a == linefeedChar = LineFeed
		  | a == tabChar = Tab
		  | otherwise = Unknown

getToken :: Parser Token
getToken = do {
	r <- oneOf [spaceChar, linefeedChar, tabChar] <|> anyChar;
	return (toToken r);
}

spaceToken :: Parser Token
spaceToken = do {
	r <- char spaceChar;
	return Space;
}

getTokens = many getToken

-- tokenToDigit :: Parser [Token]
tokenToDigit = do {
	sign <- getToken;
	if sign == Space then
		do {
			i <- getToken;
			rest <- getToken;
			return ((2^i) + rest);
		}
		else do 
			return 0;
	-- case i of
	-- 	Space -> do {
	-- 		rest <- getTokens;
	-- 		return (Space:rest);
			
	-- 	}
	-- 	_ -> do {
	-- 		rest <- getTokens;
	-- 		return (i:rest)
	-- 	}
}

-- program = do {
-- 	i <- getToken;
-- 	o <- getToken;
-- 	case (i, o) of
-- 		(Space, Space) -> stack;
-- 		_ -> other;
-- 	-- return (i, o);
-- }

-- stack = do {
-- 	i <- getToken;
-- 	case i of
-- 		Space -> do {
-- 			-- r <- stack;
-- 			return ('0':[]);
-- 		}
-- 		Tab -> do {
-- 			-- r <- stack;
-- 			return ('1':[]);
-- 		}
-- 		_ -> other;
-- }
-- other = do {
-- 	return "";
-- }