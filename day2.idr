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

-- Part 1

data Shape = Rock | Paper | Scissors

parseShape : String -> Maybe Shape
parseShape "A" = Just Rock
parseShape "B" = Just Paper
parseShape "C" = Just Scissors
parseShape "X" = Just Rock
parseShape "Y" = Just Paper
parseShape "Z" = Just Scissors
parseShape _ = Nothing

Round : Type
Round = (Shape, Shape)

parseRound : String -> Maybe Round
parseRound s = case forget (split (== ' ') s) of
  [shape1, shape2] => MkPair <$> parseShape shape1 <*> parseShape shape2
  _ => Nothing

parseRounds : String -> Maybe (List Round)
parseRounds = traverse parseRound . lines

data Outcome = Win | Draw | Loss

roundOutcome : (opponentShape, ourShape : Shape) -> Outcome
roundOutcome Rock Paper = Win
roundOutcome Rock Scissors = Loss
roundOutcome Paper Rock = Loss
roundOutcome Paper Scissors = Win
roundOutcome Scissors Rock = Win
roundOutcome Scissors Paper = Loss
roundOutcome _ _ = Draw

scoreOutcome : Outcome -> Integer
scoreOutcome Win = 6
scoreOutcome Draw = 3
scoreOutcome Loss = 0

scoreShape : Shape -> Integer
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

scoreRound : Round -> Integer
scoreRound (opponentShape, ourShape) = 
  scoreShape ourShape + scoreOutcome (roundOutcome opponentShape ourShape)

solve' : List Round -> Integer
solve' = sum . map scoreRound

solve : String -> Maybe Integer
solve = map solve' . parseRounds

-- Part 2

parseOutcome : String -> Maybe Outcome
parseOutcome "X" = Just Loss
parseOutcome "Y" = Just Draw
parseOutcome "Z" = Just Win
parseOutcome _ = Nothing

Strategy : Type
Strategy = (Shape, Outcome)

parseStrategy : String -> Maybe Strategy
parseStrategy s = case forget (split (== ' ') s) of
  [shape, outcome] => MkPair <$> parseShape shape <*> parseOutcome outcome
  _ => Nothing

sameShapeIsADraw : {shape : _} -> roundOutcome shape shape = Draw
sameShapeIsADraw {shape} = 
  case shape of
    Rock => Refl
    Paper => Refl
    Scissors => Refl

chooseShape : (opponentShape : Shape) -> (desiredOutcome : Outcome) -> 
  (ourShape : Shape ** roundOutcome opponentShape ourShape = desiredOutcome)
chooseShape shape Draw = (shape ** sameShapeIsADraw)
chooseShape Rock Win = (Paper ** Refl)
chooseShape Rock Loss = (Scissors ** Refl)
chooseShape Paper Win = (Scissors ** Refl)
chooseShape Paper Loss = (Rock ** Refl)
chooseShape Scissors Win = (Rock ** Refl)
chooseShape Scissors Loss = (Paper ** Refl)

scoreStrategy : Strategy -> Integer
scoreStrategy (opponentShape, desiredOutcome) = 
  let (ourShape ** _) = chooseShape opponentShape desiredOutcome in
  scoreShape ourShape + scoreOutcome desiredOutcome

solve2' : List Strategy -> Integer
solve2' = sum . map scoreStrategy

solve2 : String -> Maybe Integer
solve2 = map solve2' . traverse parseStrategy . lines

-- Driver

main : IO ()
main = do
  Right contents <- readFile "day2.txt" | Left error => die ("Error reading file: " ++ show error)
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: " ++ show answer1)
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: " ++ show answer2)

