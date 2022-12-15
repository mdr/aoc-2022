module Plfa.Induction

import Control.Relation

%default total

plusIsAssociative : (m, n, p : Nat) -> (m + n) + p = m + (n + p)
plusIsAssociative Z n p = Refl
plusIsAssociative (S m') n p = cong S (plusIsAssociative m' n p)

zeroRightIdentityForPlus : (n : Nat) -> n + 0 = n
zeroRightIdentityForPlus Z = Refl
zeroRightIdentityForPlus (S n') = cong S (zeroRightIdentityForPlus n')

succPlus : (m, n : Nat) -> m + S n = S (m + n)
succPlus Z n = Refl
succPlus (S m') n = cong S (succPlus m' n)

plusIsCommutative : (m, n : Nat) -> m + n = n + m
plusIsCommutative Z n = rewrite (zeroRightIdentityForPlus n) in Refl
plusIsCommutative (S m') n = 
  let IHNm' = plusIsCommutative m' n
  in trans (cong S IHNm') (sym (succPlus n m'))

plusIsCommutativeAlt : (m, n : Nat) -> m + n = n + m
plusIsCommutativeAlt m Z = zeroRightIdentityForPlus m
plusIsCommutativeAlt m (S n') = 
  let IHn' = plusIsCommutativeAlt m n'
  in trans (succPlus m n') (cong S IHn')

rearrange : (m, n, p, q : Nat) -> (m + n) + (p + q) = m + (n + p) + q
rearrange m n p q = 
  let
    eq1 = the ((m + n) + (p + q) = ((m + n) + p) + q)                   $ sym (plusIsAssociative (m + n) p q)
    eq2 = the                     (((m + n) + p) + q = m + (n + p) + q) $ cong (+ q) (plusIsAssociative m n p)
  in
    the       ((m + n) + (p + q)                     = m + (n + p) + q) $ trans eq1 eq2

-- Commutativity with rewrite
zeroRightIdentityForPlus' : (n : Nat) -> n + 0 = n
zeroRightIdentityForPlus' Z = Refl
zeroRightIdentityForPlus' (S n') = rewrite zeroRightIdentityForPlus' n' in Refl

succPlus' : (m, n : Nat) -> m + S n = S (m + n)
succPlus' Z n = Refl
succPlus' (S m') n = rewrite succPlus' m' n in Refl

plusIsCommutative' : (m, n : Nat) -> m + n = n + m
plusIsCommutative' m Z = rewrite zeroRightIdentityForPlus' m in Refl
plusIsCommutative' m (S n') = rewrite succPlus' m n' in rewrite plusIsCommutative' m n' in Refl

plusSwap : (m, n, p : Nat) -> m + (n + p) = n + (m + p)
plusSwap m n p = 
  let
    eq1 = the (m + (n + p) = (m + n) + p) $ sym (plusIsAssociative m n p)
    eq2 = the           (_ = (n + m) + p) $ cong (+ p) (plusIsCommutative m n)
    eq3 = the           (_ = n + (m + p)) $ plusIsAssociative n m p
  in
    (eq1 `trans` eq2) `trans` eq3

multiplyDistributesOverPlus : (m, n, p : Nat) -> (m + n) * p = m * p + n * p
multiplyDistributesOverPlus Z n p = Refl
multiplyDistributesOverPlus (S m') n p = 
  rewrite multiplyDistributesOverPlus m' n p in 
    rewrite plusIsAssociative p (m' * p) (n * p) in 
      Refl

multiplyIsAssociative : (m, n, p : Nat) -> (m * n) * p = m * (n * p)
multiplyIsAssociative Z n p = Refl
multiplyIsAssociative (S m') n p = 
  rewrite (sym $ multiplyIsAssociative m' n p) in 
    multiplyDistributesOverPlus n (m' * n) p

multZeroIsZero : (m : Nat) -> m * 0 = 0
multZeroIsZero Z = Refl
multZeroIsZero (S m') = rewrite multZeroIsZero m' in Refl

plusMultS : (m, n : Nat) -> m * S n = m + m * n
plusMultS Z n = Refl
plusMultS (S m') n = 
  rewrite plusMultS m' n in 
    rewrite plusSwap n m' (m' * n) in 
      Refl

multiplyIsCommutative : (m, n : Nat) -> m * n = n * m
multiplyIsCommutative Z n = sym $ multZeroIsZero n
multiplyIsCommutative (S m') n = 
  rewrite multiplyIsCommutative m' n in 
    sym $ plusMultS n m'

monus : (m, n : Nat) -> Nat
monus m Z = m
monus Z (S n') = Z
monus (S m') (S n') = m' `monus` n'

zeroMonusIsZero : (m : Nat) -> 0 `monus` m = 0
zeroMonusIsZero Z = Refl
zeroMonusIsZero (S m') = rewrite sym $ zeroMonusIsZero m' in Refl

monusAssociatesWithPlus : (m, n, p : Nat) -> (m `monus` n) `monus` p = m `monus` (n + p)
monusAssociatesWithPlus Z n p =
  rewrite zeroMonusIsZero n in
    rewrite zeroMonusIsZero (n + p) in
      rewrite zeroMonusIsZero p in Refl
monusAssociatesWithPlus (S m') Z p = Refl
monusAssociatesWithPlus (S m') (S n') p = monusAssociatesWithPlus m' n' p

infixr 10 ^
(^) : (m, n : Nat) -> Nat
m ^ 0 = 1
m ^ (S n') = m * (m ^ n')

expDistributesOverPlus : (m, n, p : Nat) -> m ^ (n + p) = (m ^ n) * (m ^ p)
expDistributesOverPlus m Z p = rewrite zeroRightIdentityForPlus (m ^ p) in Refl
expDistributesOverPlus m (S n') p = 
  rewrite expDistributesOverPlus m n' p in 
    rewrite multiplyIsAssociative m (m ^ n') (m ^ p) in
      Refl

-- https://github.com/frex-project/idris-frex/blob/main/src/Data/Relation/Closure/ReflexiveTransitive.idr
data RTList : Rel a -> Rel a where
  Nil  : RTList r x x
  (::) : {0 r : Rel a} -> {y : a}
          -> r x y -> RTList r y z
          -> RTList r x z

rtrans : Reflexive a r => Transitive a r => {x, y : a} -> RTList r x y -> r x y
rtrans Nil = reflexive
rtrans (a :: b) = a `transitive` rtrans b
      
expDistributesOverMultiply : (m, n, p : Nat) -> (m * n) ^ p = (m ^ p) * (n ^ p)
expDistributesOverMultiply m n Z = Refl
expDistributesOverMultiply m n (S p') =
  rewrite expDistributesOverMultiply m n p' in
    let
      eq1 = the ((m * n) * (m ^ p' * n ^ p') = m * (n * (m ^ p' * n ^ p'))) $ multiplyIsAssociative _ _ _
      eq2 = the (_ =                           m * ((n * m ^ p') * n ^ p')) $ rewrite multiplyIsAssociative n (m ^ p') (n ^ p') in Refl
      eq3 = the (m * ((n * m ^ p') * n ^ p') = m * ((m ^ p' * n) * n ^ p')) $ rewrite multiplyIsCommutative n (m ^ p') in Refl
      eq4 = the (m * ((m ^ p' * n) * n ^ p') = m * (m ^ p' * (n * n ^ p'))) $ rewrite multiplyIsAssociative (m ^ p') n (n ^ p') in Refl
      eq5 = the (m * (m ^ p' * (n * n ^ p')) = (m * m ^ p') * (n * n ^ p')) $ rewrite multiplyIsAssociative m (m ^ p') (n * n ^ p') in Refl
    in 
            the ((m * n) * (m ^ p' * n ^ p') = (m * m ^ p') * (n * n ^ p')) $ rtrans [eq1, eq2, eq3, eq4, eq5]

exp1Is1 : (p : Nat) -> 1 ^ p = 1
exp1Is1 Z = Refl
exp1Is1 (S p') = rewrite exp1Is1 p' in Refl

expExpIsExpMult : (m, n, p : Nat) -> (m ^ n) ^ p = m ^ (n * p)
expExpIsExpMult m Z p = rewrite exp1Is1 p in Refl
expExpIsExpMult m (S n') p =
    rewrite expDistributesOverMultiply m (m ^ n') p in
      rewrite expExpIsExpMult m n' p in 
        rewrite expDistributesOverPlus m p (n' * p) in
          Refl
