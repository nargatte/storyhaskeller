module CodeParser (parseShgameFile, parseShsaveFile)
where

import Text.Parsec.String
import Text.Parsec
import Data.Char
import Control.Monad
import CodeStructure
import qualified State as State
import qualified Data.Map as Map
import System.IO

parseFromFileUTF8 :: Parser a -> String -> IO (Either ParseError a)
parseFromFileUTF8 p fname
    = do 
        h <- openFile fname ReadMode
        hSetEncoding h utf8
        input <- hGetContents h
        return (runP p () fname input)

parseShgameFile :: String -> IO (Either ParseError CodeBlocks)
parseShgameFile = parseFromFileUTF8 $
    do
    x <- ignoreHead $ codeBlocks
    eof
    return x

parseShsaveFile :: String -> IO (Either ParseError State.State)
parseShsaveFile = parseFromFileUTF8 $
    do
    x <- (many $ noneOf " ") `sepBy` char ' '
    eof
    return $ State.stringsToState x

-- | Ignore following whitespaces
lexeme :: Parser a -> Parser a
lexeme p = 
    do
    x <- p
    spaces
    return x

parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
   where leftOver = manyTill anyToken eof

oneLineComment :: Parser ()
oneLineComment = lexeme $ string "//" >> manyTill anyChar (try $ (eof <|> (void endOfLine))) >> return ()

multiLineComment :: Parser ()
multiLineComment = lexeme $ (string "/*") >> manyTill anyChar (try $ string "*/") >> return ()
    
comments :: Parser ()
comments = many (try oneLineComment <|> multiLineComment) >> return ()

-- | Ignore following whitespaces and comments
cStyle :: Parser a -> Parser a
cStyle p = 
    do
    c <- p
    spaces
    comments
    return c 

keyWord :: String -> Parser ()
keyWord s = string s >> (cStyle $ space) >> return ()

ignoreHead :: Parser a -> Parser a
ignoreHead p = spaces >> comments >> p

tagName :: Parser String
tagName = cStyle $ many1 $ satisfy (\a -> isDigit a || isLetter a || a == '_')

tagNames :: Parser [String]
tagNames = sepBy1 tagName $ cStyle $ char ','

braces :: Parser a -> Parser a
braces p = 
    do
    void $ cStyle $ char '{'
    c <- p
    void $ cStyle $ char '}'
    return c

codeBlocks :: Parser CodeBlocks
codeBlocks = 
    do
    cbs <- many codeBlock
    return $ Map.fromList cbs
    
codeBlock :: Parser (CodeBlockName, CodeBlock)
codeBlock 
      = try (codeBlock' "frame" FrameBlock)
    <|> try (codeBlock' "dialog" DialogBlock)
    <|> codeBlock' "procedure" ProcedureBlock
        where
        codeBlock' :: String -> (Instructions -> CodeBlock) -> Parser (CodeBlockName, CodeBlock)
        codeBlock' key con =
            do
            keyWord key
            n <- tagName
            ins <- braces $ instructions
            return (n, con ins)

instructions :: Parser Instructions
instructions = concat <$> (many $ oneOpticalInstruction)

oneOpticalInstruction :: Parser Instructions
oneOpticalInstruction = 
        try (commandAndTagNames "set" Set)
    <|> try (commandAndTagNames "unset" Unset)
    <|> (instruction >>= return . (:[]))

instruction :: Parser Instruction
instruction
      = try (commandAndTagName "execute" Execute)
    <|> try (commandAndTagName "go" Go)
    <|> try returnInstruction
    <|> try escape
    <|> try tell
    <|> fullIf


commandAndTagName :: String -> (String -> Instruction) -> Parser Instruction
commandAndTagName key con =
    do
    keyWord key
    n <- tagName
    return $ con n

commandAndTagNames :: String -> (String -> Instruction) -> Parser Instructions
commandAndTagNames key con =
    do
    keyWord key
    n <- tagNames
    return $ map con n

returnInstruction :: Parser Instruction
returnInstruction = keyWord "return" >> return Return

-- | Style where if there are only one instruction, braces are not need
bracesInstructionsOrSingle :: Parser Instructions
bracesInstructionsOrSingle
        = try (braces instructions)
        <|> oneOpticalInstruction

-- | Proces string like latex + must be in "" and allow to use \" and \n
stringExpresion :: Parser String
stringExpresion =
    do
    void $ char '\"'
    spaces 
    ss <- cStyle $ manyTill stringExp $ char '\"'
    return $ concat ss
        where
        stringExp :: Parser String
        stringExp =
            do
            w <- many $ noneOf " \\\"\n"
            s <-    try newLineSymbol
                <|> try apostropheSymbol
                <|> try newLines
                <|> newLine1
                <|> spaces'
                <|> ((lookAhead $ char '\"') >> return "")
            return $ w ++ s

        newLineSymbol :: Parser String
        newLineSymbol = spaces >> string "\\n" >> spaces >> return "\n"

        apostropheSymbol :: Parser String
        apostropheSymbol = string "\\\"" >> return "\""

        newLine1 :: Parser String
        newLine1 = endOfLine >> return ""

        newLines :: Parser String -- oh God
        newLines = many (char ' ') >> endOfLine >> many (char ' ') >> many1 (endOfLine >> many (char ' ')) >> return "\n"

        spaces' :: Parser String
        spaces' = (many1 $ char ' ') >> return " " 

escape :: Parser Instruction
escape = 
    do
    keyWord "escape"
    com <-  stringExpresion
    ins <- bracesInstructionsOrSingle
    return $ Escape com ins

tell :: Parser Instruction
tell =
    do
    keyWord "tell"
    s <- stringExpresion
    return $ Tell s

fullIf :: Parser Instruction
fullIf 
    =   try ifElse
    <|> try ifNotElse
    <|> try onlyIf
    <|> onlyIfNot
        where
        onlyIf :: Parser Instruction
        onlyIf = 
            do
            keyWord "if"
            n <- tagName
            ins <- bracesInstructionsOrSingle
            return $ If n ins []

        onlyIfNot :: Parser Instruction
        onlyIfNot = 
            do
            keyWord "ifnot"
            n <- tagName
            ins <- bracesInstructionsOrSingle
            return $ If n [] ins

        ifElse :: Parser Instruction
        ifElse = 
            do
            If n trueIns _ <- onlyIf
            keyWord "else"
            falseIns <- bracesInstructionsOrSingle
            return $ If n trueIns falseIns

        ifNotElse :: Parser Instruction
        ifNotElse =
            do
            If n _ falseIns <- onlyIfNot
            keyWord "else"
            trueIns <- bracesInstructionsOrSingle
            return $ If n trueIns falseIns
    

