module Types.Number where
import qualified Types.Tokens as TT
data NumberT = NumberTCons TT.Character NumberT | NumberTCon TT.Character