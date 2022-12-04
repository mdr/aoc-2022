module Day4

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
  2-4,6-8
  2-3,4-5
  5-7,7-9
  2-8,3-7
  6-6,4-6
  2-6,4-8
"""

-- Part 1

SectionId : Type
SectionId = Nat

SectionAssignment : Type
SectionAssignment = (SectionId, SectionId)

SectionAssignmentPair : Type
SectionAssignmentPair = (SectionAssignment, SectionAssignment)

parseSectionAssignment : String -> Maybe SectionAssignment
parseSectionAssignment s = do
  let [s1, s2] = forget $ split (== '-') s | _ => Nothing
  id1 <- parsePositive s1
  id2 <- parsePositive s2
  Just (id1, id2)

parseSectionAssignmentPair : String -> Maybe SectionAssignmentPair
parseSectionAssignmentPair s = do
  let [s1, s2] = forget $ split (== ',') s | _ => Nothing
  assignment1 <- parseSectionAssignment s1
  assignment2 <- parseSectionAssignment s2
  Just (assignment1, assignment2)

parseSectionAssignments : String -> Maybe (List SectionAssignmentPair)
parseSectionAssignments = traverse parseSectionAssignmentPair . lines

fullyContains : SectionAssignment -> SectionAssignment -> Bool
fullyContains (a, b) (c, d) = a <= c && b >= d

needsReconsideration : SectionAssignment -> SectionAssignment -> Bool
needsReconsideration assignment1 assignment2 =
  fullyContains assignment1 assignment2 || fullyContains assignment2 assignment1

solve' : List SectionAssignmentPair -> Nat
solve' = count (uncurry needsReconsideration)

solve : String -> Maybe Nat
solve = map solve' . parseSectionAssignments

exampleWorks : solve Day4.example = Just 2
exampleWorks = Refl

-- Part 2

overlap : SectionAssignment -> SectionAssignment -> Bool
overlap (a, b) (c, d) = a <= c && c <= b || c <= a && a <= d

solve2' : List SectionAssignmentPair -> Nat
solve2' = count (uncurry overlap)

solve2 : String -> Maybe Nat
solve2 = map solve2' . parseSectionAssignments

exampleWorks2 : solve2 Day4.example = Just 4
exampleWorks2 = Refl

-- Driver

main : IO ()
main = do
  Right contents <- readFile "day4.txt" | Left error => die ("Error reading file: \{show error}")
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}")
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}")
