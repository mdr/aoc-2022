module Main

import Data.List
import Data.List1
import Data.Nat
import Data.SortedSet
import Data.String
import System
import System.File
import Utils

example = """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""

-- Part 1

Compartment : Type
Compartment = List Char

Rucksack : Type
Rucksack = (Compartment, Compartment)

parseRucksack : String -> Rucksack
parseRucksack s = 
  let middle = divNatNZ (length s) 2 SIsNonZero in
  splitAt middle (unpack s)

priority : Char -> Integer
priority item = cast $ ord item - ord (if isUpper item then 'A' else 'a') + (if isUpper item then 27 else 1)

solveRucksack : Rucksack -> Integer
solveRucksack (compartment1, compartment2) = 
  let overlap = SortedSet.toList (fromList compartment1 `intersection` fromList compartment2) in
    sumBy priority overlap

solve' : List Rucksack -> Integer
solve' = sumBy solveRucksack

solve : String -> Integer
solve = solve' . map parseRucksack . lines 

-- Part 2

-- Driver

main : IO ()
main = do
  Right contents <- readFile "day3.txt" | Left error => die ("Error reading file: \{show error}")
  let answer1 = solve contents
  putStrLn ("Part 1: \{show answer1}")