module DayX

import Data.List
import Data.List1
import Data.Nat
import Data.SortedSet
import Data.String
import Data.Vect
import System
import System.File
import Utils

%default total
%prefix_record_projections off

example = """
"""

-- Part 1

PuzzleInput : Type
PuzzleInput = String

parsePuzzleInput : String -> Maybe PuzzleInput

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
  contents <- readDay X
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}")
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}")
