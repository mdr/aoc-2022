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

record SectionAssignment where
  constructor MkSectionAssignment
  id1, id2 : SectionId
  isValidRange : LTE id1 id2

SectionAssignmentPair : Type
SectionAssignmentPair = (SectionAssignment, SectionAssignment)

parseSectionAssignment : String -> Maybe SectionAssignment
parseSectionAssignment s = do
  let [s1, s2] = forget $ split (== '-') s | _ => Nothing
  id1 <- parsePositive s1
  id2 <- parsePositive s2
  let Yes isValidRange = isLTE id1 id2 | No _ => Nothing
  Just (MkSectionAssignment id1 id2 isValidRange)

parseSectionAssignmentPair : String -> Maybe SectionAssignmentPair
parseSectionAssignmentPair s = do
  let [s1, s2] = forget $ split (== ',') s | _ => Nothing
  assignment1 <- parseSectionAssignment s1
  assignment2 <- parseSectionAssignment s2
  Just (assignment1, assignment2)

parseSectionAssignments : String -> Maybe (List SectionAssignmentPair)
parseSectionAssignments = traverse parseSectionAssignmentPair . lines

containsId : SectionAssignment -> SectionId -> Bool
containsId assignment id = assignment.id1 <= id && id <= assignment.id2

fullyContains : SectionAssignment -> SectionAssignment -> Bool
fullyContains assignment1 assignment2 = 
  (assignment1 `containsId` assignment2.id1) && (assignment1 `containsId` assignment2.id2)

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

disjoint : SectionAssignment -> SectionAssignment -> Bool
disjoint assignment1 assignment2 = 
  assignment1.id2 < assignment2.id1 || assignment1.id1 > assignment2.id2

overlap : SectionAssignment -> SectionAssignment -> Bool
overlap = (not .) . disjoint

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
