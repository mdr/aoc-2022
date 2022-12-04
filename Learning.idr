module Learning

import Utils
import Data.Vect
import Decidable.Equality

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : Num numType => (numargs : Nat) -> numType -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k (acc + next)

data Format = Number Format | Str Format | Lit String Format | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (s : String) -> PrintfType fmt
PrintfType (Lit s fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Lit s fmt) acc = printfFmt fmt (acc ++ s)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: xs) = Number (toFormat xs)
toFormat ('%' :: 's' :: xs) = Str (toFormat xs)
--toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = 
  case toFormat chars of
    Lit lit chars' => Lit (strCons c lit) chars'
    fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt (toFormat (unpack fmt)) ""

checkEqNat' : (n : Nat) -> (m : Nat) -> Maybe (n = m)
checkEqNat' Z Z = Just Refl
checkEqNat' (S n) (S m) = checkEqNat' n m |> map (cong S)
checkEqNat' _ _ = Nothing

myReverseSlow : Vect n a -> Vect n a
myReverseSlow {n = Z} [] = []
myReverseSlow {n = S k} (x :: xs) = 
  let result = myReverseSlow xs ++ [x] in 
    rewrite plusCommutative 1 k in result

append : Vect n a -> Vect m a -> Vect (m + n) a
append [] ys = rewrite plusZeroRightNeutral m in ys
append {n = S k} {m} (x :: xs) ys =
  let result = x :: append xs ys in 
    rewrite sym $ plusSuccRightSucc m k in result

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym $ plusZeroRightNeutral m
myPlusCommutes (S n) m = 
  rewrite sym $ plusSuccRightSucc m n in 
    cong S (myPlusCommutes n m)

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where
    reverse' : Vect p a -> Vect q a -> Vect (p + q) a
    reverse' acc [] = rewrite plusZeroRightNeutral p in acc
    reverse' {p} {q = S k} acc (x :: xs) = 
      rewrite sym $ plusSuccRightSucc p k in 
        (reverse' (x :: acc) xs)

twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible        

zeroNotSucc : Not (0 = S k)
zeroNotSucc Refl impossible

succNotZero : Not (S k = 0)
succNotZero Refl impossible

noRec : (contra : Not (k = l)) -> Not (S k = S l)
noRec contra Refl = contra (Refl)

checkEqNat : (n : Nat) -> (m : Nat) -> Dec (n = m)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSucc
checkEqNat (S k) Z = No succNotZero
checkEqNat (S k) (S l) =
  case checkEqNat k l of
    Yes Refl => Yes Refl
    No contra => No (noRec contra)

data Elem : a -> Vect k a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

total
Uninhabited (Elem a []) where
  uninhabited Here impossible
  uninhabited (There _) impossible

removeElem : {n : _} -> (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElem value (value :: xs) {prf = Here} = xs
removeElem {n = Z} value [x] {prf = There later} = absurd later
removeElem {n = S k} value (x :: xs) {prf = There later} = x :: removeElem value xs

notInTail : (notHere: Not (value = x)) -> (notThere : Not (Elem value xs)) -> Not (Elem value (x :: xs))
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem : DecEq ty => (value : ty) -> (xs : Vect n ty) -> Dec (Elem value xs)
isElem value [] = No uninhabited
isElem value (x :: xs) =
  case decEq value x of
    Yes Refl => Yes Here
    No notHere => case isElem value xs of
      Yes later => Yes (There later)
      No notThere => No (notInTail notHere notThere)