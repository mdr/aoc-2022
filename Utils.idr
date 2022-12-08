module Utils

import Data.List1
import Data.SortedMap
import Data.SortedSet
import Data.String
import Data.Vect
import System
import System.File

%default total

infixl 4 |>

public export total
(|>) : a -> (a -> b) -> b
a |> f = f a

partial
public export
chunksOf : Nat -> List a -> List (List a)
chunksOf n xs = case splitAt n xs of
  (ys, []) => [ys]
  (ys, zs) => ys :: chunksOf n zs

-- simpler total chunking as suggested by ohad in Idris discord:
public export total
chunksOf3 : List a -> List (Vect 3 a)
chunksOf3 (a :: b :: c :: rest) = [a, b, c] :: chunksOf3 rest
chunksOf3 _ = []

public export total
sumBy : Num b => Foldable t => (a -> b) -> t a -> b
sumBy f = foldl (\x, y => x + f y) 0

public export total
maxList : Ord ty => List ty -> Maybe ty
maxList = map (foldr1 max) . List1.fromList

public export total
minimum : Ord ty => List ty -> Maybe ty
minimum = map (foldr1 min) . List1.fromList

public export total
getLineGroups : String -> List (List String)
getLineGroups = forget . split (== "") . lines . trim

public export total
liftM2 : Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do
  x1 <- m1
  x2 <- m2
  pure (f x1 x2)

public export
splitString : Char -> String -> List String
splitString c s = forget $ split (== c) s

public export
sortedMapFromPairs : (Ord k) => List (k, v) -> SortedMap k v
sortedMapFromPairs = foldl (\m, (k, v) => insert k v m) empty

public export
size : SortedMap k v -> Nat
size = length . values

public export
unionAll : (Foldable f, Ord a) => f (SortedSet a) -> SortedSet a
unionAll = foldl SortedSet.union SortedSet.empty

public export
map : (Ord b) => (a -> b) -> SortedSet a -> SortedSet b
map f = SortedSet.fromList . map f . SortedSet.toList

public export
iterate : Monad m => (n : Nat) -> (a -> m a) -> a -> m a
iterate 0 _ a = pure a
iterate (S n) f x = f x >>= iterate n f

public export
allDifferent : Eq a => List a -> Bool
allDifferent xs = xs == nub xs

public export
slidingWindows : (n : Nat) -> List a -> {auto prf : NonZero n} -> List (List a)
slidingWindows n [] = []
slidingWindows (S n) (x :: xs) {prf = SIsNonZero} = 
  let 
    window = x :: take n xs
  in
    if length window == S n then window :: slidingWindows (S n) xs else []

public export
zipWithIndex : List a -> List (Integer, a)
zipWithIndex = zipWithIndex' 0
  where
    zipWithIndex' : (index : Integer) -> (List a) -> List (Integer, a)
    zipWithIndex' index [] = []
    zipWithIndex' index (x :: xs) = (index, x) :: zipWithIndex' (index + 1) xs
      
partial
export 
readDay : Fin 26 -> IO (String)
readDay n = do
  Right contents <- readFile "inputs/day\{show n}.txt" | Left error => die ("Error reading file: " ++ show error)
  pure contents
