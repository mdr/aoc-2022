module Day7

import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
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

-- Parsing

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

-- Filesystem & Zipper

data Node : Type where
  File : (size : Integer) -> Node
  Directory : (entries : SortedMap String Node) -> Node

data Context : Type where
  Top : Context
  Path : (name : String) -> (context: Context) -> (siblings : SortedMap String Node) -> Context

Location : Type
Location = (Node, Context)

toNode : Location -> Node
toNode (node, Top) = node
toNode (node, Path name context siblings) = 
  toNode (insert name node siblings |> Directory, context)

top : Node -> Location
top node = (node, Top)

cd : String -> Location -> Maybe Location
cd _ (File _, context) = Nothing
cd name (Directory entries, context) = do
  node <- lookup name entries
  let siblings = delete name entries
  Just (node, Path name context siblings)

up : Location -> Maybe Location
up (node, Top) = Nothing
up (node, Path name context siblings) = 
  let node' = insert name node siblings
  in Just (Directory node', context)

upmost : Location -> Location
upmost (node, Top) = (node, Top)
upmost (node, Path name context siblings) = upmost (Directory (insert name node siblings), context)

mkdir : String -> Node -> Maybe Node
mkdir name (File _) = Nothing
mkdir name (Directory entries) = 
  case lookup name entries of
    Just _ => Nothing
    Nothing => Directory (insert name (Directory SortedMap.empty) entries) |> Just

addFile : (name : String) -> (size : Integer) -> Node -> Maybe Node
addFile _ _ (File _) = Nothing
addFile name size (Directory entries) = 
   (insert name (File size) entries) |> Directory |> Just 

modify : (Node -> Maybe Node) -> Location -> Maybe Location
modify f (node, context) = f node |> map (, context)

root = Directory SortedMap.empty

-- Run commands

processListingItem : ListingItem -> Location -> Maybe Location
processListingItem (DirItem name) location = modify (mkdir name) location
processListingItem (FileItem size name) location = modify (addFile name size) location

executeCommand : Command -> Location -> Maybe Location
executeCommand (Cd name) location = cd name location
executeCommand CdUp location = up location
executeCommand CdRoot location = Just (upmost location)
executeCommand (Ls listing) location = foldlM (flip processListingItem) location listing

executeCommands : List Command -> Maybe Node
executeCommands commands = foldlM (flip executeCommand) (top root) commands |> map toNode

-- scan for directories with total sizes

getSize : Node -> Integer
getSize (File size) = size
getSize (Directory entries) = foldl (\acc, node => acc + getSize node) 0 entries

getDirectories : Node -> List Node
getDirectories (File _) = []
getDirectories directory@(Directory entries) = directory :: (values entries >>= getDirectories)

findAndSumAllSmallDirectories : Node -> Integer
findAndSumAllSmallDirectories node = 
  getDirectories node |> map getSize |> filter (<= 100000) |> sum

solve' : PuzzleInput -> Maybe Integer
solve' commands = executeCommands commands |> map findAndSumAllSmallDirectories

solve : String -> Maybe Integer
solve = parsePuzzleInput >=> solve'

-- Part 2

totalDiskSpace : Integer
totalDiskSpace = 70000000

targetAvailableDiskSpace : Integer
targetAvailableDiskSpace = 30000000

solve2' : PuzzleInput -> Maybe Integer
solve2' commands = do
  node <- executeCommands commands
  let availableDiskSpace = totalDiskSpace - getSize node
  let shortfall = targetAvailableDiskSpace - availableDiskSpace
  getDirectories node |> map getSize |> filter (>= shortfall) |> minList

solve2 : String -> Maybe Integer
solve2 = parsePuzzleInput >=> solve2'

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 7
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}")
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}")
