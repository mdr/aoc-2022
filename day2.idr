module Main

import Data.List
import Data.List1
import Data.String
import System
import System.File
import Utils

example = """
A Y
B X
C Z
"""

data HandShape = Rock | Paper | Scissors

parseHandShape : String -> Maybe HandShape
parseHandShape "A" = Just Rock
parseHandShape "B" = Just Paper
parseHandShape "C" = Just Scissors
parseHandShape "X" = Just Rock
parseHandShape "Y" = Just Paper
parseHandShape "Z" = Just Scissors
parseHandShape _ = Nothing

Round : Type
Round = (HandShape, HandShape)

parseRound : String -> Maybe Round
parseRound s = case forget (split (== ' ') s) of
  [a, b] => liftM2 MkPair (parseHandShape a) (parseHandShape b)
  _ => Nothing

parseRounds : String -> Maybe (List Round)
parseRounds = traverse parseRound . lines

data RoundOutcome = Win | Draw | Loss

roundOutcome : Round -> RoundOutcome
roundOutcome (Rock, Paper) = Win
roundOutcome (Rock, Scissors) = Loss
roundOutcome (Paper, Rock) = Loss
roundOutcome (Paper, Scissors) = Win
roundOutcome (Scissors, Rock) = Win
roundOutcome (Scissors, Paper) = Loss
roundOutcome _ = Draw

scoreOutcome : RoundOutcome -> Integer
scoreOutcome Win = 6
scoreOutcome Draw = 3
scoreOutcome Loss = 0

scoreShape : HandShape -> Integer
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

scoreRound : Round -> Integer
scoreRound (a, b) = scoreShape b + scoreOutcome (roundOutcome (a, b))

solve' : List Round -> Integer
solve' = sum . map scoreRound

solve : String -> Maybe Integer
solve = map solve' . parseRounds

-- Part 2

parseOutcome : String -> Maybe RoundOutcome
parseOutcome "X" = Just Loss
parseOutcome "Y" = Just Draw
parseOutcome "Z" = Just Win
parseOutcome _ = Nothing

RoundStrategy : Type
RoundStrategy = (HandShape, RoundOutcome)

parseRoundStrategy : String -> Maybe RoundStrategy
parseRoundStrategy s = case forget (split (== ' ') s) of
  [a, b] => liftM2 MkPair (parseHandShape a) (parseOutcome b)
  _ => Nothing

chooseShape : RoundStrategy -> HandShape
chooseShape (shape, Draw) = shape
chooseShape (Rock, Win) = Paper
chooseShape (Rock, Loss) = Scissors
chooseShape (Paper, Win) = Scissors
chooseShape (Paper, Loss) = Rock
chooseShape (Scissors, Win) = Rock
chooseShape (Scissors, Loss) = Paper

scoreRoundStrategy : RoundStrategy -> Integer
scoreRoundStrategy strategy@(shape, outcome) = 
  scoreShape (chooseShape strategy) + scoreOutcome outcome

solve2' : List RoundStrategy -> Integer
solve2' = sum . map scoreRoundStrategy

solve2 : String -> Maybe Integer
solve2 = map solve2' . traverse parseRoundStrategy . lines

-- Driver

main : IO ()
main = do
  Right contents <- readFile "day2.txt" | Left error => die ("Error reading file: " ++ show error)
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: " ++ show answer1)
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: " ++ show answer2)

