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

-- Part 1

Height : Type
Height = Fin 10

TreeGrid : (rows, columns : Nat) -> Type
TreeGrid rows columns = Vect rows (Vect columns Height)

%prefix_record_projections off

record TreeGridWithBounds where
  constructor MkTreeGridWithBounds
  rows, columns : Nat
  grid : TreeGrid rows columns

parseHeight : Char -> Maybe Height
parseHeight c = integerToFin (cast (ord c - ord '0')) 10

decToMaybe : Dec prop -> Maybe prop
decToMaybe (Yes prf) = Just prf
decToMaybe (No _) = Nothing

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

Point : Nat -> Nat -> Type
Point rows columns = (Fin rows, Fin columns)

visibleIndices : Vect columns (Fin columns, Height) -> SortedSet (Fin columns)
visibleIndices = visibleIndices' {maxHeightSoFar = Nothing}
  where
    visibleIndices' : (maxHeightSoFar : Maybe Height) -> Vect cols (Fin columns, Height) -> SortedSet (Fin columns)
    visibleIndices' maxHeightSoFar Nil = SortedSet.empty
    visibleIndices' maxHeightSoFar ((column, height) :: heights) =
      case maxHeightSoFar of
        Nothing => SortedSet.insert column (visibleIndices' (Just height) heights)
        Just maxHeightSoFar => 
          if height > maxHeightSoFar then
            SortedSet.insert column (visibleIndices' (Just height) heights)
          else
            visibleIndices' (Just maxHeightSoFar) heights

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

solve' : {columns : _} -> TreeGrid rows columns -> Integer
solve' = visibleInGrid .> SortedSet.toList .> length .> cast

solve : String -> Maybe Integer
solve s = do
  treeGrid <- parseTreeGrid s
  let (MkTreeGridWithBounds rows columns grid) = treeGrid
  Just (solve' grid)

-- Part 2

solve2' : TreeGrid rows columns -> Maybe Integer

solve2 : String -> Maybe Integer

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 8
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}") -- 1849
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}") -- 201600
