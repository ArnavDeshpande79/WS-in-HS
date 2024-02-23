module Utils (
	safeHead,
	safeTail
) where

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

safeTail :: [a] -> [a]
safeTail (_:y) = y
safeTail [] = []