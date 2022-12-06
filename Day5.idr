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
  show instruction = "move " ++ show (n instruction) ++ " from " ++ show (from instruction) ++ " to " ++ show (to instruction)
  
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

sortedMapFromPairs : (Ord k) => List (k, v) -> SortedMap k v
sortedMapFromPairs = foldl (\m, (k, v) => insert k v m) empty

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

popStack : Stack -> Maybe (Crate, Stack)
popStack [] = Nothing
popStack (x :: xs) = Just (x, xs)

pushStack : Crate -> Stack -> Stack
pushStack x xs = x :: xs

updateMapBy : k -> (v -> v) -> SortedMap k v -> SortedMap k v
updateMapBy k f m = case lookup k m of
  Nothing => m
  Just v => insert k (f v) m

moveCrateFromTo : (from, to: Nat) -> State -> Maybe State
moveCrateFromTo from to state = do
  fromStack <- lookup from state
  (crate, fromStack') <- popStack fromStack
  toStack <- lookup to state
  let toStack' = pushStack crate toStack
  let newState = state |> insert from fromStack' |> insert to toStack'
  Just newState

moveCratesFromTo : (n, from, to: Nat) -> State -> Maybe State
moveCratesFromTo 0 from to state = Just state
moveCratesFromTo (S k) from to state = do
  state' <- moveCrateFromTo from to state
  moveCratesFromTo k from to state'

applyInstruction : Instruction -> State -> State
applyInstruction instruction state = 
  case moveCratesFromTo (n instruction) (from instruction) (to instruction) state of
    Nothing => state
    Just state' => state'

size : SortedMap k v -> Nat
size = length . values

getTopOfStacks : State -> List Crate
getTopOfStacks state =
  [1..size state] |> map (\i => lookup i state) |> catMaybes |> map head' |> catMaybes

solve' : PuzzleInput -> String
solve' (state, instructions) = 
  let 
    finalState = foldl (flip applyInstruction) state instructions
    topOfStacks = getTopOfStacks finalState
  in
    pack topOfStacks

solve : String -> Maybe String
solve = map solve' . parsePuzzleInput

-- Part 2

popStack' : Nat -> Stack -> (Stack, Stack)
popStack' n stack = (stack |> take n, stack |> drop n)

pushStack' : Stack -> Stack -> Stack
pushStack' xs ys = xs ++ ys

moveCratesFromTo' : (n, from, to: Nat) -> State -> Maybe State
moveCratesFromTo' n from to state = do
  fromStack <- lookup from state
  let (liftedCrates, fromStack') = popStack' n fromStack
  toStack <- lookup to state
  let toStack' = pushStack' liftedCrates toStack
  let newState = state |> insert from fromStack' |> insert to toStack'
  Just newState


applyInstruction' : Instruction -> State -> State
applyInstruction' instruction state = 
  case moveCratesFromTo' (n instruction) (from instruction) (to instruction) state of
    Nothing => state
    Just state' => state'

solve2' : PuzzleInput -> String
solve2' (state, instructions) = 
  let 
    finalState = foldl (flip applyInstruction') state instructions
    topOfStacks = getTopOfStacks finalState
  in
    pack topOfStacks

solve2 : String -> Maybe String
solve2 = map solve2' . parsePuzzleInput
  
-- Driver

main : IO ()
main = do
  contents <- readDay 5
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}")
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}")
