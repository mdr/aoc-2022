module Utils

import Data.List1
import Data.String

infixl 9 |>

export
(|>) : a -> (a -> b) -> b
a |> f = f a

export -- total
chunksOf : Nat -> List a -> List (List a)
chunksOf n xs = case splitAt n xs of
  (ys, []) => [ys]
  (ys, zs) => ys :: chunksOf n zs

export 
sumBy : Num b => (a -> b) -> List a -> b
sumBy f = foldl (\x, y => x + f y) 0

export 
maxList : Ord ty => List ty -> Maybe ty
maxList = map (foldr1 max) . List1.fromList

export 
getLineGroups : String -> List (List String)
getLineGroups = forget . split (== "") . lines . trim

export 
liftM2 : Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do
  x1 <- m1
  x2 <- m2
  pure (f x1 x2)