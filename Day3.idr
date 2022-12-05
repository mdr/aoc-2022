module Day3

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

Item : Type
Item = Char

Compartment : Type
Compartment = List Item

Rucksack : Type
Rucksack = (Compartment, Compartment)

parseRucksack : String -> Rucksack
parseRucksack s = 
  let middle = divNatNZ (length s) 2 SIsNonZero in
      splitAt middle (unpack s)

priority : Item -> Integer
priority item with (isLower item)
  priority item | True = cast $ ord item - ord 'a' + 1
  priority item | False = cast $ ord item - ord 'A' + 27
      
solveRucksack : Rucksack -> Integer
solveRucksack (compartment1, compartment2) = 
  let
    set1 = SortedSet.fromList compartment1
    set2 = SortedSet.fromList compartment2
    overlap = intersection set1 set2    
  in
    sumBy priority overlap

solve' : List Rucksack -> Integer
solve' = sumBy solveRucksack

parseRucksacks : String -> List Rucksack
parseRucksacks = map parseRucksack . lines

solve : String -> Integer
solve = solve' . parseRucksacks

-- Part 2

Rucksack' : Type
Rucksack' = List Item

ElfGroup : Type
ElfGroup = Vect 3 Rucksack'

getBadgePriority : ElfGroup -> Integer
getBadgePriority = sumBy priority . SortedSet.toList . foldl1 intersection . map SortedSet.fromList

solve2' : List ElfGroup -> Integer
solve2' = sumBy getBadgePriority

parseElfGroups : String -> List ElfGroup
parseElfGroups = chunksOf3 . map unpack . lines

solve2 : String -> Integer
solve2 = solve2' . parseElfGroups

-- Driver

main : IO ()
main = do
  Right contents <- readFile "day3.txt" | Left error => die ("Error reading file: \{show error}")
  let answer1 = solve contents
  putStrLn ("Part 1: \{show answer1}")
  let answer2 = solve2 contents
  putStrLn ("Part 2: \{show answer2}")
