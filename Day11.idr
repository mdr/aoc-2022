module Day11

import Control.Monad.State
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

MonkeyId : Nat -> Type
MonkeyId = Fin

WorryLevel : Type
WorryLevel = Integer

record MonkeyDescription (n : Nat) where
  constructor MkMonkeyDescription
  id : MonkeyId n
  items : List WorryLevel
  operation : WorryLevel -> WorryLevel
  test : WorryLevel -> Bool
  ifTrue : MonkeyId n
  ifFalse : MonkeyId n

-- Parsing

parseMonkeyId : (n : Nat) -> String -> Maybe (MonkeyId n)
parseMonkeyId n s = do
  let [_, s2] = words s | _ => Nothing
  id <- s2 |> liftToString (filter isDigit) |> parsePositive
  natToFin id n

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

parseIfTrue : (n : Nat) -> String -> Maybe (MonkeyId n)
parseIfTrue n s = do
  let ["If", "true:", "throw", "to", "monkey", s6] = words s | _ => Nothing
  id <- parsePositive s6
  natToFin id n

parseIfFalse : (n : Nat) -> String -> Maybe (MonkeyId n)
parseIfFalse n s = do
  let ["If", "false:", "throw", "to", "monkey", s6] = words s | _ => Nothing
  id <- parsePositive s6
  natToFin id n

parseMonkeyDescription : (n : Nat) -> List String -> Maybe (MonkeyDescription n)
parseMonkeyDescription n lines = do
  let [s1, s2, s3, s4, s5, s6] = lines | _ => Nothing
  id <- parseMonkeyId n s1
  startingItems <- parseStartingItems s2
  operation <- parseOperation s3
  test <- parseTest s4
  ifTrue <- parseIfTrue n s5
  ifFalse <- parseIfFalse n s6
  pure $ MkMonkeyDescription id startingItems operation test ifTrue ifFalse

parseMonkeyDescriptions : String -> Maybe (n ** Vect n (MonkeyDescription n))
parseMonkeyDescriptions s = do
  let (n ** lineGroups) = s |> getLineGroups |> listToVect
  monkeyDescriptions <- traverse (parseMonkeyDescription n) lineGroups
  Just (n ** monkeyDescriptions)

-- Part 1

MonkeyState : Nat -> Type
MonkeyState n = Vect n (MonkeyDescription n)

giveItemToMonkey : WorryLevel -> MonkeyDescription n -> MonkeyDescription n
giveItemToMonkey item = { items $= (++ [item]) }

giveItemToMonkey2 : WorryLevel -> MonkeyId n -> MonkeyState n -> MonkeyState n
giveItemToMonkey2 item monkeyId = updateAt monkeyId (giveItemToMonkey item)

handleItem : MonkeyDescription n -> WorryLevel -> (WorryLevel, MonkeyId n)
handleItem monkey worryLevel = 
  let 
    newWorryLevel = monkey.operation worryLevel `div` 3
    nextMonkeyId = if monkey.test newWorryLevel then monkey.ifTrue else monkey.ifFalse
  in 
    (newWorryLevel, nextMonkeyId)

processItems : MonkeyDescription n -> List WorryLevel -> MonkeyState n -> MonkeyState n
processItems monkey Nil state = state
processItems monkey (item :: items) state = 
  let
    (newWorryLevel, nextMonkeyId) = handleItem monkey item
    newState = giveItemToMonkey2 newWorryLevel nextMonkeyId state
  in
    processItems monkey items newState

performTurn : MonkeyId n -> MonkeyState n -> MonkeyState n
performTurn monkeyId state = 
  let 
    monkey = index monkeyId state
    items = monkey.items
    newState = processItems monkey items state
  in 
    updateAt monkeyId ({items := [] }) newState

performRound : MonkeyState n -> MonkeyState n
performRound state = 
  let 
    monkeys = Utils.Vect.zipWithIndex state
  in
    foldl (\state, (monkey, monkeyId) => performTurn monkeyId state) state monkeys

traceN : (a -> a) -> Nat -> a -> List a
traceN f 0 x = [x]
traceN f (S n) x = x :: traceN f n (f x)

showState : MonkeyState n -> String
showState = map (.items) .> show

run20 : MonkeyState n -> List (MonkeyState n)
run20 = traceN performRound 20 

countInspections : MonkeyState n -> Vect n Nat
countInspections = map (.items .> length)

doThing : String -> Maybe (List Nat)
doThing s = do
  (n ** state) <- parseMonkeyDescriptions s
  let states = traceN performRound 20 state
  map countInspections states |> map toList |> transpose |> map sum |> pure

solve' : Vect n (MonkeyDescription n) -> Nat

solve : String -> Maybe Nat
-- solve = map solve' . parseMonkeyDescriptions

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
