module State where

import qualified Data.Set as Set
import qualified Data.DList as DList
import CodeStructure
import Data.Maybe
import Text.Regex.Posix
import Data.List

type PreviousBlock = String
type CurrentBlock = String
type NextBlock = Maybe String

type EscapeList = DList.DList (String, Instructions)

type TextBuffor = DList.DList String

type Flags = Set.Set String

type State = (PreviousBlock, CurrentBlock, NextBlock, Flags, EscapeList, TextBuffor)

new :: State
new = ("main", "main", Nothing, Set.empty, DList.empty, DList.empty)

currentBlock :: State -> String
currentBlock (_, c, _, _, _, _) = c

nextBlock :: State -> String
nextBlock (_, _, Just n, _, _, _) = n
nextBlock (_, c, Nothing, _, _, _) = c

nextFrame :: State -> State
nextFrame (_, cb, Just nb, fs, _, _) = (cb, nb, Nothing, fs, DList.empty, DList.empty)
nextFrame (_, cb, Nothing, fs, _, _) = (cb, cb, Nothing, fs, DList.empty, DList.empty)

nextDialog :: State -> State
nextDialog (pb, cb, Just nb, fs, _, _) = (pb, nb, Nothing, fs, DList.empty, DList.empty)
nextDialog (pb, cb, Nothing, fs, _, _) = (pb, cb, Nothing, fs, DList.empty, DList.empty)

addFlag :: State -> String -> State
addFlag (pb, cb, Just nb, fs, el, tb) _ = (pb, cb, Just nb, fs, el, tb)
addFlag (pb, cb, Nothing, fs, el, tb) flag = (pb, cb, Nothing, Set.insert flag fs, el, tb)

deleteFlag :: State -> String -> State
deleteFlag (pb, cb, Just nb, fs, el, tb) _ = (pb, cb, Just nb, fs, el, tb)
deleteFlag (pb, cb, Nothing, fs, el, tb) flag = (pb, cb, Nothing, Set.delete flag fs, el, tb)

addText :: State -> String -> State
addText (pb, cb, Just nb, fs, el, tb) _ = (pb, cb, Just nb, fs, el, tb)
addText (pb, cb, Nothing, fs, el, tb) text = (pb, cb, Nothing, fs, el, DList.append tb $ DList.singleton text)

addEscape :: State -> String -> Instructions -> State
addEscape (pb, cb, Just nb, fs, el, tb) _ _ = (pb, cb, Just nb, fs, el, tb)
addEscape (pb, cb, Nothing, fs, el, tb) name ins = (pb, cb, Nothing, fs, DList.append el $ DList.singleton (name, ins), tb)

go :: State -> String -> State
go (pb, cb, Just nb, fs, el, tb) _ = (pb, cb, Just nb, fs, el, tb)
go (pb, cb, Nothing, fs, el, tb) nextBlock = (pb, cb, Just nextBlock, fs, el, tb)

goReturn :: State -> State
goReturn (pb, cb, Just nb, fs, el, tb) = (pb, cb, Just nb, fs, el, tb)
goReturn (pb, cb, Nothing, fs, el, tb) = (pb, cb, Just pb, fs, el, tb)

text :: State -> String
text (_, _, _, _, _, tb) = concat $ DList.toList tb

isFlag :: State -> String -> Bool
isFlag (_, _, _, fs, _, _) flag = Set.member flag fs

matchEscape :: State -> String -> [String]
matchEscape (_, _, _, _, el, _) s = filter (=~ (".*" ++ s ++ ".*") ) $ map fst $ DList.toList el

escapeInstructions :: State -> String -> Instructions
escapeInstructions (_, _, _, _, el, _) s = fromJust $ lookup s $ DList.toList el

stateToString :: State -> String
stateToString (pb, cb, _, fs, _, _) = pb ++ " " ++ cb ++ " " ++ (concat $ intersperse " " $ Set.toList fs)

stringsToState :: [String] -> State
stringsToState [] = new
stringsToState (_:[]) = new
stringsToState (f:s:ss) = (f, s, Nothing, Set.fromList ss, DList.empty, DList.empty)

