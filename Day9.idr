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
  rope : List1 Point
  tailVisited : SortedSet Point

origin : Point
origin = (0, 0)

replicate1 : (n : Nat) -> a -> {auto prf : NonZero n} -> List1 a
replicate1 (S n) x {prf = SIsNonZero} = x ::: replicate n x

makeInitialState : (knots : Nat) -> {auto prf : NonZero knots} -> State
makeInitialState knots = 
  let
    rope = replicate1 knots origin
    tail = last rope
  in
    MkState rope (singleton tail)

Semigroup Integer where
  (<+>) = (+)

getDelta : Direction -> Vector
getDelta R = (1, 0)
getDelta U = (0, 1)
getDelta L = (-1, 0)
getDelta D = (0, -1)

updateHead : (a -> a) -> List1 a -> List1 a
updateHead f (x ::: xs) = f x ::: xs

updateTail : (List a -> List a) -> List1 a -> List1 a
updateTail f (x ::: xs) = x ::: f xs

updateRope : (List1 Point -> List1 Point) -> State -> State
updateRope f = { rope $= f }

moveHead : Direction -> State -> State
moveHead direction = updateRope (updateHead (<+> getDelta direction)) 

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

updateFollowers : (leader : Point) -> (followers : List Point) -> List Point
updateFollowers leader [] = []
updateFollowers leader (follower :: followers) =
  let 
    updatedFollower = follow leader follower
  in 
    updatedFollower :: updateFollowers updatedFollower followers
    
updateFollowers1 : (rope : List1 Point) -> List1 Point
updateFollowers1 (head ::: followers) = head ::: updateFollowers head followers

tailFollow : State -> State
tailFollow state@(MkState rope tailVisited) = 
  let 
    updatedRope = updateFollowers1 rope
  in 
    { rope := updatedRope, tailVisited := insert (last updatedRope) tailVisited } state

moveAndFollow : Direction -> State -> State
moveAndFollow direction = tailFollow . moveHead direction

handleMotion : Motion -> State -> State
handleMotion (direction, distance) = iterate distance (moveAndFollow direction)

solveWith : (initialState : State) -> List Motion -> Nat
solveWith initialState = foldl (flip handleMotion) initialState .> .tailVisited .> size

solve' : List Motion -> Nat
solve' = solveWith (makeInitialState 2)

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
solve2' = solveWith (makeInitialState 10)

solve2 : String -> Maybe Nat
solve2 = map solve2' . parseMotions

-- Driver

partial
main : IO ()
main = do
  contents <- readDay 9
  let Just answer1 = solve contents | Nothing => die "Error solving puzzle 1"
  putStrLn ("Part 1: \{show answer1}") -- 6269
  let Just answer2 = solve2 contents | Nothing => die "Error solving puzzle 2"
  putStrLn ("Part 2: \{show answer2}") -- 2557