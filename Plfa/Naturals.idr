module Plfa.Naturals

%default total

data Bin = Zero | O Bin | I Bin

inc : Bin -> Bin
inc Zero = I Zero
inc (O b) = I b
inc (I b) = O (inc b)

eleven : Bin
eleven = I $ I $ O $ I $ O $ Zero

toBin : Nat -> Bin
toBin Z = O Zero
toBin (S n) = inc (toBin n)

toNat : Bin -> Nat
toNat Zero = Z
toNat (O b) = 2 * toNat b
toNat (I b) = 1 + 2 * toNat b