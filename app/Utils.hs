module Utils (
	safeHead,
	safeTail,
	ifThenElse
) where

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

safeTail :: [a] -> [a]
safeTail (_:y) = y
safeTail [] = []

ifThenElse :: Bool -> a -> a -> a
ifThenElse a b c = if a then b else c