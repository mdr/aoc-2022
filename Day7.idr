module Day7

import Data.List
import Data.List1
import Data.Nat
import Data.SortedSet
import Data.String
import Data.Vect
import System
import Utils

-- %default total

example = """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""

-- Part 1

data ListingItem = DirItem String | FileItem Integer String

Show ListingItem where
  show (DirItem s) = "DirItem " ++ s
  show (FileItem n s) = "FileItem " ++ show n ++ " " ++ s

Listing : Type
Listing = List ListingItem

data Command = Cd String | CdUp | CdRoot | Ls Listing

Show Command where
  show (Cd s) = "Cd " ++ s
  show CdUp = "CdUp"
  show CdRoot = "CdRoot"
  show (Ls listing) = "Ls " ++ show listing

PuzzleInput : Type
PuzzleInput = List Command

parseListingItem : String -> Maybe ListingItem
parseListingItem s = do
  let [s1, s2] = words s | _ => Nothing
  case s1 of
    "dir" => DirItem s2 |> Just
    _ => parseInteger s1 |> map (\n => FileItem n s2) 

parseCommand : List String -> Maybe (Command, List String)
parseCommand [] = Nothing
parseCommand ("$ cd .." :: rest) = Just (CdUp, rest)
parseCommand ("$ cd /" :: rest) = Just (CdRoot, rest)
parseCommand ("$ ls" :: rest) = 
  do
    let (outputLines, rest) = span (not . isPrefixOf "$") rest
    listing <- traverse parseListingItem outputLines
    Just (Ls listing, rest)
parseCommand (command :: rest) = 
  if "$ cd " `isPrefixOf` command then
    let dir = command |> unpack |> drop 5 |> pack
    in Just (Cd dir, rest)
  else 
    Nothing

parseCommands : List String -> Maybe (List Command)
parseCommands [] = Just []
parseCommands lines = 
  do
    (command, rest) <- parseCommand lines
    commands <- parseCommands rest
    Just (command :: commands)

parsePuzzleInput : String -> Maybe PuzzleInput
parsePuzzleInput = parseCommands . lines

data FileSystemNode = File String Nat | Directory String (List FileSystemNode)

emptyFileSystem = Directory "" []

solve' : PuzzleInput -> Nat

solve : String -> Maybe Nat
solve = map solve' . parsePuzzleInput

-- Part 2

solve2' : PuzzleInput -> Nat

solve2 : String -> Maybe Nat
solve2 = map solve2' . parsePuzzleInput

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 7
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}")
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}")
