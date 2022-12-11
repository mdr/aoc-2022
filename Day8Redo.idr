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

solve' : TreeGrid rows columns -> Integer

solve : String -> Maybe Integer

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
