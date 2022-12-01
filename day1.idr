module Main

import Data.List
import Data.List1
import Data.String
import System
import System.File

example = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

getLineGroups : String -> List (List String)
getLineGroups = forget . split (== "") . forget . split (== '\n') . trim

groupsToIntegers : List (List String) -> Maybe (List (List Integer))
groupsToIntegers = traverse (traverse parseInteger)

maxList : Ord ty => List ty -> Maybe ty
maxList = map (foldr1 max) . List1.fromList

solve' : List (List Integer) -> Maybe Integer
solve' elfInventories = maxList (map sum elfInventories)

solve : String -> Maybe Integer
solve = (>>= solve') . groupsToIntegers . getLineGroups

solve2 : String -> Maybe Integer
solve2 = map (sum . take 3 . reverse . sort . map sum) . groupsToIntegers . getLineGroups

main : IO ()
main = do
  Right contents <- readFile "day1.txt" | Left error => putStrLn ("Error reading file: " ++ show error)
  Just answer1 <- pure (solve contents) | Nothing => putStrLn "Error solving puzzle 1"
  putStrLn ("Part 1: " ++ show answer1)
  Just answer2 <- pure (solve2 contents) | Nothing => putStrLn "Error solving puzzle 2"
  putStrLn ("Part 2: " ++ show answer2)

