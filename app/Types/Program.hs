module Types.Program where
import qualified Types.Tokens as Token


data Program = Instruction Program | EmptyProgram deriving Show
data Instruction = IWithParam | IWithoutParam
data IWithParam = InstrWithParam IMP Parameter
data IWithoutParam = InstrWithoutParam IMP
data Parameter = Number | EmptyParameter
data IMP = Stack