-- define the abstract syntax here
module Parser.Syntax where
-- grammar definition
-- S = space
-- Instruction -> Instr Instruction | Empty
-- Instr -> IMP Operator Number


-- Abstract Syntax Tree

type Program = [Instruction]

data Instruction = StackInstr StackOperation deriving Show

data StackOperation = StackPush Int | StackDuplicate | StackCopy Int | StackSwap | StackDiscard | StackSlide Int deriving Show

				-- | ArithmeticInstr
				-- | HeapInsr
				-- | FlowInsr
				-- | IOInstr