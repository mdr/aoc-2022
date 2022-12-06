module Day5

import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.SortedSet
import Data.String
import Data.Vect
import System
import System.File
import Utils

%default total

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
State = SortedMap Nat Stack

record Instruction where
  constructor MkInstruction
  n, from, to : Nat

Show Instruction where
  show instruction = "move " ++ show instruction.n ++ " from " ++ show instruction.from ++ " to " ++ show instruction.to
  
PuzzleInput : Type
PuzzleInput = (State, List Instruction)

-- Parsing

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
  let 
    stacks = lines |> reverse |> drop 1 |> map parseCrates |> transpose |> map (reverse . catMaybes)
    ids = [1..length stacks]
  in 
    sortedMapFromPairs (ids `zip` stacks)

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

-- Part 1

popStack : Nat -> Stack -> (Stack, Stack)
popStack n stack = (stack |> take n, stack |> drop n)

pushStack : Stack -> Stack -> Stack
pushStack xs ys = xs ++ ys

moveCratesFromTo : (n, from, to: Nat) -> State -> Maybe State
moveCratesFromTo n from to state = do
  fromStack <- lookup from state
  let (liftedCrates, fromStack') = popStack n fromStack
  toStack <- lookup to state
  let toStack' = pushStack liftedCrates toStack
  let newState = state |> insert from fromStack' |> insert to toStack'
  Just newState

moveCratesOneAtAtTime : (n, from, to: Nat) -> State -> Maybe State
moveCratesOneAtAtTime n from to = iterate n (moveCratesFromTo 1 from to)

applyInstruction : Instruction -> State -> State
applyInstruction instruction state = 
  moveCratesOneAtAtTime instruction.n instruction.from instruction.to state |> fromMaybe state

readTopOfStacks : State -> String
readTopOfStacks state =
  [1..size state] |> mapMaybe (flip lookup state) |> mapMaybe head' |> pack

solveWith : (Instruction -> State -> State) -> PuzzleInput -> String
solveWith handleInstruction (state, instructions) = 
  let 
    finalState = foldl (flip handleInstruction) state instructions
  in
    readTopOfStacks finalState

solve' : PuzzleInput -> String
solve' = solveWith applyInstruction

solve : String -> Maybe String
solve = map solve' . parsePuzzleInput

-- Part 2

applyInstruction2 : Instruction -> State -> State
applyInstruction2 instruction state = 
   moveCratesFromTo instruction.n instruction.from instruction.to state |> fromMaybe state

solve2' : PuzzleInput -> String
solve2' = solveWith applyInstruction2

solve2 : String -> Maybe String
solve2 = map solve2' . parsePuzzleInput
  
-- Driver

partial
main : IO ()
main = do
  contents <- readDay 5
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}") -- "QPJPLMNNR"
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}") -- "BQDNWJPVJ"
