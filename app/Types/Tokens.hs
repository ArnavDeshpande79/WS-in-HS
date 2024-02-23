module Types.Tokens where
import qualified Types.Types as Types

data Token = IMP [Types.Character]
		   | Operator [Types.Character]
		   | Parameter [Types.Character] deriving Show