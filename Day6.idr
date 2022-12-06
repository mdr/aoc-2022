module Day6

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

example = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

examples : List String
examples = [
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
  "bvwbjplbgvbhsrlpgdmjqwftvncz", -- first marker after character 5
  "nppdvjthqldpwncqszvftbrmjlhg", -- first marker after character 6
  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", -- first marker after character 10
  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" -- first marker after character 11
]

-- Part 1

solve' : (n: Nat) -> {auto prf : NonZero n} -> String -> Maybe Nat
solve' n s =
  do
    let windows = slidingWindows n (unpack s)
    index <- findIndex allDifferent windows
    Just (cast index + n)

solve : String -> Maybe Nat
solve = solve' 4

-- Part 2

solve2 : String -> Maybe Nat
solve2 = solve' 14

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 6
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}")
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}")
