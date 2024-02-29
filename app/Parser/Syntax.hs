-- define the abstract syntax here
module Parser.Syntax where
-- grammar definition
-- S = space
-- Instruction -> Instr Instruction | Empty
-- Instr -> IMP Operator Number


-- Abstract Syntax Tree

type Program = [Instruction]

data Instruction = StackInstr StackOperation
				 | ArithmeticInstr ArithmeticOperation
				 | HeapInstr HeapInstruction
				 | FlowInstr FlowInstruction
				 | IOInstr IOInstruction deriving Show

data StackOperation = StackPush Int
					| StackDuplicate
					| StackCopy Int
					| StackSwap
					| StackDiscard
					| StackSlide Int deriving Show

data ArithmeticOperation = Addition
						 | Subtraction
						 | Multiplication
						 | IntegerDivision
						 | Modulo deriving Show

data HeapInstruction = Store
					 | Retrieve deriving Show

data FlowInstruction = Mark Int
					 | Call Int
					 | Jump Int
					 | JumpIfZero Int
					 | JumpIfNegative Int
					 | SubRoutineEnd
					 | ProgramEnd deriving Show

data IOInstruction = OutputChar
				   | OutputInt
				   | ReadCharacter
				   | ReadInt deriving Show