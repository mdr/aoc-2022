module Day11

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
%prefix_record_projections off

example = """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"""

MonkeyId : Type
MonkeyId = Nat

WorryLevel : Type
WorryLevel = Integer

record MonkeyDescription where
  constructor MkMonkeyDescription
  id : MonkeyId
  items : List WorryLevel
  operation : WorryLevel -> WorryLevel
  test : WorryLevel -> Bool
  ifTrue : MonkeyId
  ifFalse : MonkeyId

parseMonkeyId : String -> Maybe MonkeyId
parseMonkeyId s = do
  let [_, s2] = words s | _ => Nothing
  s2 |> liftToString (filter isDigit) |> parsePositive

parseStartingItems : String -> Maybe (List WorryLevel)
parseStartingItems s = s |> words |> drop 2 |> map (liftToString (filter isDigit)) |> traverse parseInteger

parseBinOp : String -> Maybe (WorryLevel -> WorryLevel -> WorryLevel)
parseBinOp s = case s of
  "+" => pure (+)
  "-" => pure (-)
  "*" => pure (*)
  _ => Nothing

parseArg : String -> Maybe (WorryLevel -> WorryLevel)
parseArg s = case s of
  "old" => pure id
  n => parseInteger n |> map const

parseOperation : String -> Maybe (WorryLevel -> WorryLevel)
parseOperation s = do
  let ["Operation:", "new", "=", s4, s5, s6] = words s | _ => Nothing
  arg1 <- parseArg s4
  op <- parseBinOp s5
  arg2 <- parseArg s6
  pure $ \old => op (arg1 old) (arg2 old)

parseTest : String -> Maybe (WorryLevel -> Bool)
parseTest s = do
  let ["Test:", "divisible", "by", s4] = words s | _ => Nothing
  divisor <- parseInteger s4
  pure $ \n => n `mod` divisor == 0

parseIfTrue : String -> Maybe MonkeyId
parseIfTrue s = do
  let ["If", "true:", "throw", "to", "monkey", s6] = words s | _ => Nothing
  parsePositive s6

parseIfFalse : String -> Maybe MonkeyId
parseIfFalse s = do
  let ["If", "false:", "throw", "to", "monkey", s6] = words s | _ => Nothing
  parsePositive s6

parseMonkeyDescription : List String -> Maybe MonkeyDescription
parseMonkeyDescription lines = do
  let [s1, s2, s3, s4, s5, s6] = lines | _ => Nothing
  id <- parseMonkeyId s1
  startingItems <- parseStartingItems s2
  operation <- parseOperation s3
  test <- parseTest s4
  ifTrue <- parseIfTrue s5
  ifFalse <- parseIfFalse s6
  pure $ MkMonkeyDescription id startingItems operation test ifTrue ifFalse

parseMonkeyDescriptions : String -> Maybe (List MonkeyDescription)
parseMonkeyDescriptions = getLineGroups .> traverse parseMonkeyDescription

-- Part 1

solve' : List MonkeyDescription -> Nat

solve : String -> Maybe Nat
solve = map solve' . parseMonkeyDescriptions

-- Part 2

PuzzleInput : Type
PuzzleInput = String

parsePuzzleInput : String -> Maybe PuzzleInput

solve2' : PuzzleInput -> Nat

solve2 : String -> Maybe Nat
solve2 = map solve2' . parsePuzzleInput

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 11
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}")
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}")
