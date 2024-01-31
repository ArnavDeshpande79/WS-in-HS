module Types.Tokens where
data Character = SpaceChar | TabChar | LineFeedChar | InvalidChar deriving Show
instance Eq Character where
    a == b = (tokenToCode a == tokenToCode b)
    a /= b = not (a == b)

tokenToCode :: Character -> Maybe Int
tokenToCode SpaceChar = Just 0
tokenToCode TabChar = Just 1
tokenToCode LineFeedChar = Just 2
tokenToCode InvalidChar = Nothing

charToToken :: Char -> Character
charToToken ' ' = SpaceChar
charToToken '\t' = TabChar
charToToken '\n' = LineFeedChar
charToToken _ = InvalidChar

tokenToChar :: Character -> Maybe Char
tokenToChar SpaceChar = Just ' '
tokenToChar TabChar = Just '\t'
tokenToChar LineFeedChar = Just '\n'
tokenToChar InvalidChar = Nothing

-- data Number = SpaceTab Number | SpaceTab
-- data SpaceTab = SpaceCharT | LineFeedCharT