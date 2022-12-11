module Utils

import Data.List1
import Data.SortedMap
import Data.SortedSet
import Data.String
import Data.Vect
import System
import System.File

%default total

infixl 1 |>

public export
(|>) : a -> (a -> b) -> b
a |> f = f a

infixl 1 .>

public export
(.>) : (a -> b) -> (b -> c) -> (a -> c)
f .> g = g . f

partial
public export
chunksOf : Nat -> List a -> List (List a)
chunksOf n xs = case splitAt n xs of
  (ys, []) => [ys]
  (ys, zs) => ys :: chunksOf n zs

-- simpler total chunking as suggested by ohad in Idris discord:
public export
chunksOf3 : List a -> List (Vect 3 a)
chunksOf3 (a :: b :: c :: rest) = [a, b, c] :: chunksOf3 rest
chunksOf3 _ = []

public export
sumBy : Num b => Foldable t => (a -> b) -> t a -> b
sumBy f = foldl (\x, y => x + f y) 0

public export
maximum : Ord ty => List ty -> Maybe ty
maximum = map (foldr1 max) . List1.fromList

namespace SortedSet
  public export
  maximum : Ord ty => SortedSet ty -> Maybe ty
  maximum = Utils.maximum . SortedSet.toList

public export
toSet : Foldable t => Ord a => t a -> SortedSet a
toSet = foldl (\s, x => insert x s) empty

public export
minimum : Ord ty => List ty -> Maybe ty
minimum = map (foldr1 min) . List1.fromList

public export
getLineGroups : String -> List (List String)
getLineGroups = forget . split (== "") . lines . trim

public export
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

namespace Vect
  public export
  zipWithIndex : Vect n a -> Vect n (Fin n, a)
  zipWithIndex [] = []
  zipWithIndex (x :: xs) = (0, x) :: map (\(i, x) => (FS i, x)) (zipWithIndex xs)

  public export
  (>>=) : Vect m a -> (a -> Vect n b) -> Vect (m * n) b
  as >>= f = concat (map f as)

  public export
  cartesianProduct : Vect n a -> Vect m b -> Vect (n * m) (a, b)
  cartesianProduct xs ys = 
    let result = 
      Utils.Vect.do
        x <- xs
        y <- ys
        [(x, y)]
    in
      rewrite (sym (multOneRightNeutral m)) in result

public export
cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct as bs = [(a, b) | a <- as, b <- bs]

public export
listToVect : (xs : List a) -> (p : Nat ** Vect p a)
listToVect [] = (0 ** [])
listToVect (x :: xs) = 
  let (p ** xs') = listToVect xs in
    (S p ** x :: xs')

public export
decToMaybe : Dec prop -> Maybe prop
decToMaybe (Yes prf) = Just prf
decToMaybe (No _) = Nothing

public export
allFins' : (n : Nat) -> List (Fin n)
allFins' 0 = []
allFins' (S n) = FZ :: (map FS (allFins' n))

partial
export 
readDay : Fin 26 -> IO (String)
readDay n = do
  Right contents <- readFile "inputs/day\{show n}.txt" | Left error => die ("Error reading file: " ++ show error)
  pure contents
