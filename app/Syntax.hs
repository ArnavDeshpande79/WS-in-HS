module Syntax where

type Instruction = (Code, [Argument])

type Argument = Int

data Code
	= StackPush
	| StackDuplicate
	| StackCopy
	| StackSwap
	| StackDiscard
	| StackSlide
	| Addition
	| Subtraction
	| Multiplication
	| IntegerDivision
	| Modulo
	| Store
	| Retrieve
	| Mark
	| Call
	| Jump
	| JumpIfZero
	| JumpIfNegative
	| SubRoutineEnd
	| ProgramEnd
	| OutputChar
	| OutputInt
	| ReadCharacter
	| ReadInt deriving Show

type Program = [Instruction]