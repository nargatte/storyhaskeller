module CodeStructure 
where

import qualified Data.Map as Map 
import Data.Maybe

type CodeBlocks = Map.Map CodeBlockName CodeBlock

type Instructions = [Instruction]

type FlagName = String

type CodeBlockName = String

data CodeBlock
    = FrameBlock Instructions -- ^ The common block, print text, register escapes and save itself as previous then exit
    | DialogBlock Instructions -- ^ The same as @FrameBlock@, only without previous save
    | ProcedureBlock Instructions -- ^ Like function, return in the same place when executed
    deriving (Eq,Show)

data Instruction
    = Set FlagName -- ^ Make state flag appear in system
    | Unset FlagName -- ^ Mane state flag dissapear in system 
    | If FlagName Instructions Instructions -- ^ Execute instructions in second parametr if flag is in the state otherwise instructions in third parametr 
    | Tell String -- ^ Make print string in the console
    | Execute CodeBlockName -- ^ Can be done only with procedure block 
    | Go CodeBlockName -- ^ Can be done only with frame or dialog block
    | Return -- ^ Make go to previous block
    | Escape String Instructions -- ^ Register new escape point, if it choosen then execute instructions
    deriving (Eq,Show)

codeBlockByName :: CodeBlocks -> String -> CodeBlock
codeBlockByName cbs s = findCodeBlock $ Map.lookup s cbs
    where 
    findCodeBlock :: Maybe CodeBlock -> CodeBlock
    findCodeBlock Nothing = error $ "can't find " ++ s ++ " block"
    findCodeBlock (Just cb) = cb