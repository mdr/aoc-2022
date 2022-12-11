module Day8Redo

import Decidable.Equality
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

-- Types

Height : Type
Height = Fin 10

TreeGrid : (rows, columns : Nat) -> Type
TreeGrid rows columns = Vect rows (Vect columns Height)

%prefix_record_projections off

record TreeGridWithBounds where
  constructor MkTreeGridWithBounds
  rows, columns : Nat
  grid : TreeGrid rows columns

Point : Nat -> Nat -> Type
Point rows columns = (Fin rows, Fin columns)

-- Parsing

parseHeight : Char -> Maybe Height
parseHeight c = integerToFin (cast (ord c - ord '0')) 10

parseTreeLine : (columns : Nat) -> String -> Maybe (Vect columns Height)
parseTreeLine columns s = do
  let (actualColumns ** chars) = unpack s |> listToVect
  heights <- traverse parseHeight chars
  prf <- decEq columns actualColumns |> decToMaybe
  Just $ rewrite prf in heights

parseTreeGrid : String -> Maybe TreeGridWithBounds
parseTreeGrid s = 
  do
    let (rows ** lines) = lines s |> listToVect
    firstLine <- vectToMaybe lines
    let (columns ** _) = unpack firstLine |> listToVect
    grid <- traverse (parseTreeLine columns) lines
    Just (MkTreeGridWithBounds rows columns grid)

-- Part 1

visibleIndices : Vect columns (Fin columns, Height) -> SortedSet (Fin columns)
visibleIndices = visibleIndices' {maxHeightSoFar = Nothing}
  where
    greaterThan : Maybe Height -> Maybe Height -> Bool
    greaterThan (Just a) (Just b) = a > b
    greaterThan (Just a) Nothing = True
    greaterThan Nothing _ = False

    visibleIndices' : (maxHeightSoFar : Maybe Height) -> Vect cols (Fin columns, Height) -> SortedSet (Fin columns)
    visibleIndices' maxHeightSoFar [] = SortedSet.empty
    visibleIndices' maxHeightSoFar ((column, height) :: heights) =
      if (Just height) `greaterThan` maxHeightSoFar then
        SortedSet.insert column (visibleIndices' (Just height) heights)
      else
        visibleIndices' maxHeightSoFar heights

visibleInRow : Vect columns Height -> SortedSet (Fin columns)
visibleInRow heights =
  let
    heightsWithIndex = heights |> zipWithIndex
    forwards = heightsWithIndex |> visibleIndices
    backwards = heightsWithIndex |> reverse |> visibleIndices
  in
    forwards `union` backwards

visibleInRows : TreeGrid rows columns -> SortedSet (Point rows columns)
visibleInRows grid = 
    grid |> map visibleInRow |> zipWithIndex |> map broadcastPair |> unionAll
  where
    broadcastPair : (Ord a, Ord b) => (a, SortedSet b) -> SortedSet (a, b)
    broadcastPair (a, bs) = map (a,) bs

visibleInGrid : {columns : _} -> TreeGrid rows columns -> SortedSet (Point rows columns)
visibleInGrid grid = 
  (visibleInRows grid) `union` (transpose grid |> visibleInRows |> map swap)

solve' : {columns : _} -> TreeGrid rows columns -> Nat
solve' = visibleInGrid .> SortedSet.toList .> length .> cast

solve : String -> Maybe Nat
solve s = do
  MkTreeGridWithBounds _ _ grid <- parseTreeGrid s
  Just (solve' grid)

-- Part 2

viewingDistance : Height -> List Height -> Nat
viewingDistance height [] = 0
viewingDistance height (h :: hs) = if h >= height then 1 else 1 + viewingDistance height hs

take' : Fin n -> Vect n a -> List a
take' FZ xs = []
take' (FS n') (x :: xs) = x :: take' n' xs

drop' : Fin n -> Vect n a -> List a
drop' FZ xs = toList xs
drop' (FS n') (x :: xs) = drop' n' xs

scenicScore1D : Fin len -> Vect len Height -> Nat
scenicScore1D i heights =
  let
    height = index i heights
    treesLeft = take' i heights |> reverse
    treesRight = drop' i heights |> drop 1
  in
    viewingDistance height treesLeft * viewingDistance height treesRight

scenicScore : {columns : Nat } -> TreeGrid rows columns -> (Point rows columns) -> Nat
scenicScore grid (row, column) =
  let
    horizontalTrees = index row grid
    verticalTrees = index column (transpose grid)
  in
    scenicScore1D column horizontalTrees * scenicScore1D row verticalTrees

allPoints : {rows, columns : Nat} -> SortedSet (Point rows columns)
allPoints {rows} {columns} = 
  let
    points = (allFins' rows) `cartesianProduct` (allFins' columns)
  in
    toSet points

solve2' : {rows, columns : _} -> TreeGrid rows columns -> Maybe Nat
solve2' grid = allPoints |> map (scenicScore grid) |> maximum

solve2 : String -> Maybe Nat
solve2 s = do
  MkTreeGridWithBounds _ _ grid <- parseTreeGrid s
  solve2' grid

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 8
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}") -- 1849
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}") -- 201600
