module Day11

import Control.Monad.State
import Data.List
import Data.List1
import Data.Nat
import Data.SortedMap
import Data.SortedSet
import Data.String
import Data.Vect
import Debug.Trace
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

record Monkey (n : Nat) where
  constructor MkMonkey
  id : MonkeyId n
  items : List WorryLevel
  operation : WorryLevel -> WorryLevel
  test : WorryLevel -> Bool
  ifTrue : MonkeyId n
  ifFalse : MonkeyId n
  inspectCount : Integer

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

parseMonkey : (n : Nat) -> List String -> Maybe (Monkey n)
parseMonkey n lines = do
  let [s1, s2, s3, s4, s5, s6] = lines | _ => Nothing
  id <- parseMonkeyId n s1
  startingItems <- parseStartingItems s2
  operation <- parseOperation s3
  test <- parseTest s4
  ifTrue <- parseIfTrue n s5
  ifFalse <- parseIfFalse n s6
  pure $ MkMonkey id startingItems operation test ifTrue ifFalse 0

parseMonkeys : String -> Maybe (n ** Vect n (Monkey n))
parseMonkeys s = do
  let (n ** lineGroups) = s |> getLineGroups |> listToVect
  monkeyDescriptions <- traverse (parseMonkey n) lineGroups
  Just (n ** monkeyDescriptions)

-- Part 1

Monkeys : Nat -> Type
Monkeys n = Vect n (Monkey n)

MonkeyState : Nat -> Type -> Type
MonkeyState n a = State (Monkeys n) a

throwItemToMonkey : WorryLevel -> MonkeyId n -> MonkeyState n ()
throwItemToMonkey item monkeyId = modify (updateAt monkeyId { items $= (++ [item]) })

handleItem : Monkey n -> WorryLevel -> (WorryLevel, MonkeyId n)
handleItem monkey worryLevel = 
  let 
    newWorryLevel = monkey.operation worryLevel `div` 3
    nextMonkeyId = if monkey.test newWorryLevel then monkey.ifTrue else monkey.ifFalse
  in 
    (newWorryLevel, nextMonkeyId)

incrementInspectCount : MonkeyId n -> MonkeyState n ()
incrementInspectCount monkeyId = modify (updateAt monkeyId ({ inspectCount $= (+ 1) }))

processItem : Monkey n -> WorryLevel -> MonkeyState n ()
processItem monkey item = do
  let (newWorryLevel, nextMonkeyId) = handleItem monkey item
  incrementInspectCount monkey.id
  throwItemToMonkey newWorryLevel nextMonkeyId

performTurn : MonkeyId n -> MonkeyState n ()
performTurn monkeyId = do 
  monkey <- gets (index monkeyId)
  traverse_ (processItem monkey) monkey.items
  modify (updateAt monkey.id { items := [] })

performRound : MonkeyState n ()
performRound = gets (map (.id)) >>= traverse_ performTurn

showState : Monkeys n -> String
showState = map (.items) .> show

solve' : Vect n (Monkey n) -> Integer
solve' initialState =
  let 
    (finalState, _) = runState initialState (replicateM_ 20 performRound)
    counts = finalState |> map (.inspectCount)
    monkeyBusiness = counts |> toList |> sort |> reverse |> take 2 |> product
  in
    monkeyBusiness

solve : String -> Maybe Integer
solve s = do
  (n ** state) <- parseMonkeys s
  Just $ solve' state

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
  putStrLn ("Part 1: \{show answer1}") -- 110220
  -- let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  -- putStrLn ("Part 2: \{show answer2}")
