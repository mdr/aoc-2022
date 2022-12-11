module Day9

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
%prefix_record_projections off

example = """
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""

data Direction = R | U | L | D

Motion : Type
Motion = (Direction, Nat)

-- Part 1

parseDirection : String -> Maybe Direction
parseDirection "R" = Just R
parseDirection "U" = Just U
parseDirection "L" = Just L
parseDirection "D" = Just D
parseDirection _ = Nothing

parseMotion : String -> Maybe Motion
parseMotion s = do
    let [s1, s2] = words s | _ => Nothing
    direction <- parseDirection s1
    distance <- parsePositive s2
    Just (direction, distance)

parseMotions : String -> Maybe (List Motion)
parseMotions s = s |> lines |> traverse parseMotion

Point : Type
Point = (Integer, Integer)

Vector : Type
Vector = (Integer, Integer)

record State where
  constructor MkState
  head, tail : Point
  tailVisited : SortedSet Point

origin : Point
origin = (0, 0)

initialState: State
initialState = MkState origin origin (singleton origin)

Semigroup Integer where
  (<+>) = (+)

getDelta : Direction -> Vector
getDelta R = (1, 0)
getDelta U = (0, 1)
getDelta L = (-1, 0)
getDelta D = (0, -1)

moveHead : Direction -> State -> State
moveHead direction = { head $= (<+> (getDelta direction)) }

subtract : Point -> Point -> Vector
subtract (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

sign : Integer -> Integer
sign n = 
  if n == 0 then 0
  else if n > 0 then 1
  else -1

signVector : Vector -> Vector
signVector (x, y) = (sign x, sign y)

getFollowVector : Point -> Point -> Vector
getFollowVector head tail = signVector (head `subtract` tail)

isAdjacent : Point -> Point -> Bool
isAdjacent (x1, y1) (x2, y2) = 
  let 
    dx = abs (x1 - x2)
    dy = abs (y1 - y2)
  in 
    dx <= 1 && dy <= 1

follow : Point -> Point -> Point
follow leader follower = 
  if isAdjacent leader follower then 
    follower
  else
    follower <+> getFollowVector leader follower

tailFollow : State -> State
tailFollow state@(MkState head tail tailVisited) = 
    let 
      tail' = follow head tail
      tailVisited' = insert tail' tailVisited
    in 
      { tail := tail', tailVisited := tailVisited' } state

moveAndFollow : Direction -> State -> State
moveAndFollow direction = tailFollow . moveHead direction

handleMotion : Motion -> State -> State
handleMotion (direction, distance) = iterate distance (moveAndFollow direction)

solve' : List Motion -> Nat
solve' motions = 
  let 
    finalState = foldl (flip handleMotion) initialState motions
  in 
    finalState.tailVisited |> size

solve : String -> Maybe Nat
solve = map solve' . parseMotions

-- Part 2

example2 = """
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""

solve2' : List Motion -> Nat

solve2 : String -> Maybe Nat
solve2 = map solve2' . parseMotions

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 9
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}") -- 6269
  -- let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  -- putStrLn ("Part 2: \{show answer2}")
