module Day1

import Data.List
import Data.List1
import Data.Fin
import Data.String
import System
import System.File
import Utils

%default total

example = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

ElfInventory : Type
ElfInventory = List Integer

parseInventories : String -> Maybe (List ElfInventory)
parseInventories = traverse (traverse parseInteger) . getLineGroups

solve' : List ElfInventory -> Maybe Integer
solve' = maxList . map sum

solve : String -> Maybe Integer
solve = (>>= solve') . parseInventories

solve2' : List ElfInventory -> Integer
solve2' = sum . take 3 . sortBy (flip compare) . map sum

solve2 : String -> Maybe Integer
solve2 = map solve2' . parseInventories

%default partial

main : IO ()
main = do
  contents <- readDay 1
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: " ++ show answer1)
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: " ++ show answer2)

