module Lib 
where

import CodeParser
import System.Environment
import Data.Either
import Text.Regex.Posix
import Text.Parsec
import CodeStructure
import qualified State as State
import System.IO 
import Interpreter
import Data.Maybe
import qualified Data.Map as Map

srcMain :: IO ()
srcMain = do
    args <- getArgs
    errorArgumentsPrintOrStartGame $ getFilesName args

getFilesName :: [String] -> Either String (String, String)
getFilesName [] = Left "Put game file (.shg) as first argument and eventualy save file (.shs) as second argument"
getFilesName (g:[]) = if g =~ ".*\\.shg" then Right (g, (take ((length g) - 4) g) ++ ".shs" ) else Left $ g ++ " is not game file (.shg)"
getFilesName (g:s:_) = if g =~ ".*\\.shg" then if s =~ ".*\\.shs" then Right (g, s) else Left $ s ++ " is not save file (.shg)" else  Left $ g ++ " is not game file (.shg)"

errorArgumentsPrintOrStartGame :: Either String (String, String) -> IO()
errorArgumentsPrintOrStartGame (Left info) = putStrLn info
errorArgumentsPrintOrStartGame (Right d) = parseGame d

parseGame :: (String, String) -> IO ()
parseGame (gFile, sFile) = do
    game <- parseShgameFile gFile
    handle <- openFile sFile ReadWriteMode
    hClose handle
    save <- parseShsaveFile sFile
    errorParserPrintOrStartGame game save
        where
        errorParserPrintOrStartGame :: Either ParseError CodeBlocks -> Either ParseError State.State -> IO ()
        errorParserPrintOrStartGame (Left info) _ = putStrLn $ "Code parser failed:\n\n" ++ show info
        errorParserPrintOrStartGame _ (Left info) = putStrLn $ "Save parser failed:\n\n" ++ show info
        errorParserPrintOrStartGame (Right cb) (Right s) = gameIterations cb s

        saveGame :: State.State -> IO ()
        saveGame state = writeFile sFile $ State.stateToString state

        gameIterations :: CodeBlocks -> State.State -> IO()
        gameIterations cb s = do
            saveGame s
            putStrLn "^^^^^^^^^^^^^^^^^^^"
            putStrLn $ (State.text aplicatedState)
            printMathes ""
            mString <- matchingCommand
            case mString of
                Nothing -> return ()
                Just fullCom -> gameIterations cb $ nextState $ applyInstructions aplicatedState cb $ State.escapeInstructions aplicatedState fullCom
            where 
            aplicatedState :: State.State
            aplicatedState = applyInstructions s cb $ blockInstructions $ codeBlockByName cb $ State.currentBlock s

            blockInstructions :: CodeBlock -> Instructions
            blockInstructions (FrameBlock ins) = ins 
            blockInstructions (DialogBlock ins) = ins 
            blockInstructions _ = error "go can only be done on frame or dialog block"

            printMathes :: String -> IO()
            printMathes str = (putStrLn "-------------------") >> (sequence_ $ map (putStrLn . (++) "- ") $ State.matchEscape aplicatedState str)

            matchingCommand :: IO (Maybe String)
            matchingCommand = do
                command <- getLine
                if command == "exit" 
                then return Nothing
                else if (length (State.matchEscape aplicatedState command)) == 1
                then return $ Just ((State.matchEscape aplicatedState command) !! 0)
                else printMathes command >> matchingCommand

            nextState :: State.State -> State.State
            nextState sta = nextBlockOperation $ codeBlockByName cb $ State.nextBlock s 
                where 
                nextBlockOperation :: CodeBlock -> State.State
                nextBlockOperation (FrameBlock _) = State.nextFrame sta
                nextBlockOperation (DialogBlock _) = State.nextDialog sta
                nextBlockOperation _ = error "go can only be done on frame or dialog block"