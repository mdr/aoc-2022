module Day5

import Data.List
import Data.List1
import Data.Nat
import Data.SortedSet
import Data.String
import Data.Vect
import System
import System.File
import Utils

example = """
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""

-- Part 1

Crate : Type
Crate = Char

Stack : Type
Stack = List Crate

State : Type
State = List Stack

record Instruction where
  constructor MkInstruction
  n, from, to : Nat

PuzzleInput : Type
PuzzleInput = (State, List Instruction)

crateAt : String -> Nat -> Maybe Crate
crateAt s n = case s |> unpack |> drop n of
  [] => Nothing
  ' ' :: _ => Nothing
  crate :: _ => Just crate

parseCrates : String -> List (Maybe Crate)
parseCrates s = 
  [0..(length s + 1) `div` 4] 
  |> reverse 
  |> drop 1 
  |> reverse 
  |> map (\i => crateAt s (i * 4 + 1))

parseState : List String -> State
parseState lines =
  lines |> reverse |> drop 1 |> map parseCrates |> transpose |> map (reverse . catMaybes)

parseInstruction : String -> Maybe Instruction
parseInstruction s = do
  let ["move", nStr, "from", fromStr, "to", toStr] = words s | _ => Nothing
  n <- parsePositive nStr
  from <- parsePositive fromStr
  to <- parsePositive toStr
  Just (MkInstruction n from to)

parseInstructions : List String -> Maybe (List Instruction)
parseInstructions = traverse parseInstruction

parsePuzzleInput : String -> Maybe PuzzleInput
parsePuzzleInput s = do
  let [stateLines, instructionsLines] = s |> lines |> split (== "") |> forget | _ => Nothing
  let state = parseState stateLines
  instructions <- parseInstructions instructionsLines  
  Just (state, instructions)

applyInstruction : Instruction -> State -> State

popCrate : Nat -> State -> Maybe (Crate, State)
popCrate n state = 
  case inBounds n state of
    Yes inBoundsProof => 
      let
        stack = index n state
      in 
        ?hole
    No _ => Nothing

moveCrateFromTo : Nat -> Nat -> State -> State
-- moveCrate from to state = 
  
solve' : PuzzleInput -> String

solve : String -> Maybe String
solve = map solve' . parsePuzzleInput

-- Part 2

-- solve2 : String -> Maybe Nat
-- solve2 = map solve2' . parseSectionAssignments

-- Driver

main : IO ()
main = do
  Right contents <- readFile "day5.txt" | Left error => die ("Error reading file: \{show error}")
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}")
  -- let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  -- putStrLn ("Part 2: \{show answer2}")
