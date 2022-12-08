module Day8

import Data.List
import Data.List1
import Data.Nat
import Data.SortedSet
import Data.String
import Data.Vect
import System
import System.File
import Utils

%default total

example = """
30373
25512
65332
33549
35390
"""

-- Part 1

Point : Type
Point = (Integer, Integer)

Height : Type
Height = Integer

TreeGrid : Type
TreeGrid = List (List Height)

parseTreeGrid : String -> Maybe TreeGrid
parseTreeGrid = lines .> map unpack .> traverse (traverse (cast .> parsePositive))

visibleIndices : List Height -> List Integer
visibleIndices = visibleIndices' {maxHeightSoFar = -1} {index = 0}
  where
    visibleIndices' : (maxHeightSoFar : Height) -> (index : Integer) -> (List Height) -> List Integer
    visibleIndices' _ _ [] = []
    visibleIndices' maxHeightSoFar index (height :: hs) =
      if height > maxHeightSoFar
      then index :: visibleIndices' height (index + 1) hs
      else visibleIndices' maxHeightSoFar (index + 1) hs

visibleInRow : List Height -> SortedSet Integer
visibleInRow heights = 
  let 
    forwards = visibleIndices heights
    width = cast (length heights)
    reverseIndex = \i => width - i - 1
    backwards = reverse heights |> visibleIndices |> map reverseIndex
  in 
    SortedSet.fromList (forwards ++ backwards)

visibleInRows : TreeGrid -> SortedSet Point
visibleInRows = map visibleInRow .> zipWithIndex .> map broadcastPair .> unionAll
  where
    broadcastPair : (Ord a, Ord b) => (a, SortedSet b) -> SortedSet (a, b)
    broadcastPair (a, bs) = map (a,) bs

visibleInGrid : TreeGrid -> SortedSet Point
visibleInGrid grid = (visibleInRows grid) `union` (transpose grid |> visibleInRows |> map swap)

solve' : TreeGrid -> Integer
solve' = visibleInGrid .> SortedSet.toList .> length .> cast

solve : String -> Maybe Integer
solve = parseTreeGrid .> map solve'

-- Part 2

solve2' : TreeGrid -> Integer

solve2 : String -> Maybe Integer
solve2 = parseTreeGrid .> map solve2'

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 8
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}") -- 1849
  -- let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  -- putStrLn ("Part 2: \{show answer2}")
