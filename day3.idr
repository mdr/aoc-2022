module Main

import Data.List
import Data.List1
import Data.Nat
import Data.SortedSet
import Data.String
import Data.Vect
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
priority item with (isLower item)
  priority item | True = cast $ ord item - ord 'a' + 1
  priority item | False = cast $ ord item - ord 'A' + 27

solveRucksack : Rucksack -> Integer
solveRucksack (compartment1, compartment2) = 
  let overlap = SortedSet.toList (fromList compartment1 `intersection` fromList compartment2) in
    sumBy priority overlap

solve' : List Rucksack -> Integer
solve' = sumBy solveRucksack

solve : String -> Integer
solve = solve' . map parseRucksack . lines 

-- Part 2

Rucksack' : Type
Rucksack' = List Char

Group : Type
Group = Vect 3 Rucksack'

solveGroup : Group -> Integer
solveGroup = sumBy priority . SortedSet.toList . foldl1 intersection . map SortedSet.fromList

solve2' : List Group -> Integer
solve2' = sumBy solveGroup

solve2 : String -> Integer
solve2 = solve2' . chunksOf3 . map unpack . lines

-- Driver

main : IO ()
main = do
  Right contents <- readFile "day3.txt" | Left error => die ("Error reading file: \{show error}")
  let answer1 = solve contents
  putStrLn ("Part 1: \{show answer1}")
  let answer2 = solve2 contents
  putStrLn ("Part 2: \{show answer2}")
