module Interpreter where

import State
import CodeStructure
import Data.List
import Data.Maybe
import qualified Data.Map as Map

applyInstructions :: State -> CodeBlocks -> Instructions -> State
applyInstructions s cb ins = foldl' applyInstruction s ins
    where
    applyInstruction :: State -> Instruction -> State
    applyInstruction sw (Set flagname)  = addFlag sw flagname
    applyInstruction sw (Unset flagname) = deleteFlag sw flagname
    applyInstruction sw (If flagname trueIns falseIns) = 
        if isFlag sw flagname
        then applyInstructions sw cb trueIns
        else applyInstructions sw cb falseIns
    applyInstruction sw (Tell text) = addText sw text
    applyInstruction sw (Execute cbName) =
        applyInstructions sw cb $ instructionsOfProcedure $ codeBlockByName cb cbName
        where
        instructionsOfProcedure :: CodeBlock -> Instructions
        instructionsOfProcedure (ProcedureBlock instructions) = instructions
        instructionsOfProcedure _ = error "execute can only be done on procedure block"
    applyInstruction sw (Go cbName) = nextByCodeBlock $ codeBlockByName cb cbName 
        where
        nextByCodeBlock :: CodeBlock -> State
        nextByCodeBlock (ProcedureBlock _) = error "go can only be done on frame or dialog block"
        nextByCodeBlock _ = go sw cbName
    applyInstruction sw Return = goReturn sw
    applyInstruction sw (Escape str insw) = addEscape sw str insw

