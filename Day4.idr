module Day4

import Data.Either
import Data.List
import Data.List1
import Data.Nat
import Data.SortedSet
import Data.String
import Data.String.Parser
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
  from, to : SectionId
  isValidRange : LTE from to

Show SectionAssignment where
  show assignment = show (from assignment) ++ "-" ++ show (to assignment)

SectionAssignmentPair : Type
SectionAssignmentPair = (SectionAssignment, SectionAssignment)

sectionAssignmentParser : Parser SectionAssignment
sectionAssignmentParser = do
  from <- natural
  skip (char '-')
  to <- natural
  let Yes isValidRange = isLTE from to | No _ => fail "invalid range \${from}-\${to}"
  pure $ MkSectionAssignment from to isValidRange

sectionAssignmentPairParser : Parser SectionAssignmentPair
sectionAssignmentPairParser = do
  assignment1 <- sectionAssignmentParser
  skip (char ',')
  assignment2 <- sectionAssignmentParser
  pure (assignment1, assignment2)

sectionAssignmentsParser : Parser (List SectionAssignmentPair)
sectionAssignmentsParser = do
  assignments <- sepBy sectionAssignmentPairParser (char '\n')
  pure assignments

parseSectionAssignments : String -> Maybe (List SectionAssignmentPair)
parseSectionAssignments = map fst . eitherToMaybe . parse sectionAssignmentsParser

containsId : SectionAssignment -> SectionId -> Bool
containsId assignment id = assignment.from <= id && id <= assignment.to

fullyContains : SectionAssignment -> SectionAssignment -> Bool
fullyContains assignment1 assignment2 = 
  (assignment1 `containsId` assignment2.from) && (assignment1 `containsId` assignment2.to)

needsReconsideration : SectionAssignment -> SectionAssignment -> Bool
needsReconsideration assignment1 assignment2 =
  (assignment1 `fullyContains` assignment2) || (assignment2 `fullyContains` assignment1)

solve' : List SectionAssignmentPair -> Nat
solve' = count (uncurry needsReconsideration)

solve : String -> Maybe Nat
solve = map solve' . parseSectionAssignments

-- Part 2

disjoint : SectionAssignment -> SectionAssignment -> Bool
disjoint assignment1 assignment2 = 
  assignment1.to < assignment2.from || assignment1.from > assignment2.to

overlap : SectionAssignment -> SectionAssignment -> Bool
overlap = (not .) . disjoint

solve2' : List SectionAssignmentPair -> Nat
solve2' = count (uncurry overlap)

solve2 : String -> Maybe Nat
solve2 = map solve2' . parseSectionAssignments

-- Driver

main : IO ()
main = do
  contents <- readDay 4
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}")
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}")
