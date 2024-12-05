module Semantics.Semantics (semProg, SemanticDomain, SemanticState) where
import qualified Parser.Syntax as Syntax

type SemanticState = ([Int],[Int], IO)
-- (stack, heap, IO)
type SemanticDomain = SemanticState -> SemanticState

-- semantic domain is a state 

-- semProg :: Syntax.Program -> SemanticDomain
-- semOp :: Syntax.Instruction -> SemanticDomain
-- semOp i (a, b) = case i of
-- 					Syntax.StackInstr s -> case b of
-- 											(x:y:xs) -> case s of
-- 															Syntax.StackSwap
-- 											(x:xs) -> 
-- 											_ -> 
-- 					_ -> (a, b)
			-- ArithmeticInstr a -> case a of
								
			-- HeapInstr h -> case h of
								
			-- FlowInstr fl -> case fl of
								
	-- IOInstr IOInstruction
{-
semantics of the whitespace program : commands that manipulate the state somehow
1. types of commands :
	stack
	arithmetic
	heap
	flow
	io
state elements:
1. stack
2. heap
3. ... ??
-}