module Day10

import Data.List
import Data.List1
import Data.Maybe
import Data.Nat
import Data.SortedSet
import Data.String
import Data.Vect
import System
import System.File
import Utils

%default total
%prefix_record_projections off

-- Part 1

example = """
noop
addx 3
addx -5
"""

example2 = """
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"""

data Instruction = Noop | Addx Integer

parseInstruction : String -> Maybe Instruction
parseInstruction "noop" = Just Noop
parseInstruction s =
  if s == "noop" then Just Noop
  else do
    let [s1, s2] = words s | _ => Nothing
    guard (s1 == "addx")
    n <- parseInteger s2
    Just (Addx n)

parseInstructions : String -> Maybe (List Instruction)
parseInstructions = lines .> traverse parseInstruction
    
State : Type
State = Integer

initialState : State
initialState = 1

traceInstruction : Instruction -> State -> List1 State
traceInstruction Noop s = singleton s
traceInstruction (Addx n) s = s ::: s + n :: []

scanlMulti : (a -> b -> List1 a) -> a -> List b -> List a
scanlMulti f x xs = x :: scanlMulti' x xs
  where
    scanlMulti' : a -> List b -> List a
    scanlMulti' x [] = []
    scanlMulti' x (y :: ys) = 
      let items = f x y in
      forget items ++ scanlMulti' (last items) ys

traceExecution : List Instruction -> List State
traceExecution = scanlMulti (flip traceInstruction) initialState

cyclesToCheck : List Nat
cyclesToCheck = [20, 60, 100, 140, 180, 220]

getSignalStrength : (stateTrace : List State) -> (cycle : Nat) -> Maybe Integer
getSignalStrength stateTrace cycle = do
  registerValue <- maybeIndex (pred cycle) stateTrace
  Just (registerValue * cast cycle)

solve' : List Instruction -> Maybe Integer
solve' instructions = do 
  let stateTrace = traceExecution instructions
  signalStrengths <- traverse (getSignalStrength stateTrace) cyclesToCheck
  Just $ sum signalStrengths

solve : String -> Maybe Integer
solve = parseInstructions >=> solve'

-- Part 2

drawPixel : (register : Integer) -> (position : Nat) -> Char
drawPixel register position = if abs (register - cast position) <= 1 then '#' else '.'

drawPixels : List (Integer, Nat) -> String
drawPixels pairs = map (uncurry drawPixel) pairs |> pack

partial
solve2' : List Instruction -> List String
solve2' instructions = traceExecution instructions |> dropRight 1 |> chunksOf 40 |> map (drawPixels . zipWithIndex)

partial
solve2 : String -> Maybe String
solve2 = parseInstructions .> map (unlines . solve2')

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 10
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}") -- 14220
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn "Part 2:"
  putStrLn answer2 -- ZRARLFZU
