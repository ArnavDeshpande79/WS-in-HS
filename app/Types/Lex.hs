module Types.Lex (
	space,
	tab,
	linefeed,
	charToToken,
) where

import qualified Types.Types as Types	

-- space = ' '
-- tab = '\t'
-- linefeed = '\n'


space = 's'
tab = 't'
linefeed = 'n'

charToToken :: Char -> Types.Character
charToToken c | (c == space) = Types.SpaceChar
			  | (c == tab) = Types.TabChar
			  | (c == linefeed) = Types.LineFeedChar
			  | otherwise = Types.InvalidChar

-- tokenToCode :: Character -> Maybe Int
-- tokenToCode SpaceChar = Just 0
-- tokenToCode TabChar = Just 1
-- tokenToCode LineFeedChar = Just 2
-- tokenToCode InvalidChar = Nothing


-- tokenToChar :: Character -> Maybe Char
-- tokenToChar SpaceChar = Just ' '
-- tokenToChar TabChar = Just '\t'
-- tokenToChar LineFeedChar = Just '\n'
-- tokenToChar InvalidChar = Nothing
