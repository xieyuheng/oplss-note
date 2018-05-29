module Day2

import Day1

%default total

data EqNat : Nat -> Nat -> Type where
  SameNat : (num : Nat) -> EqNat num num

smallProofEq : EqNat (2 + 2) 4
smallProofEq = SameNat 4

succEq : EqNat x y -> EqNat (S x) (S y)
succEq (SameNat x) = SameNat (S x)

-- a function is total
--   if for all well typed input
--   it will always give a result

-- more expressive type system
--   enable you to write more total functions
--   by being more specific about well typed input

-- `=` is the builtin
-- data (=) : a -> b -> Type where
--   Refl : x = x

smallProof : 2 + 2 = 4
smallProof = Refl

notTrue : 2 + 2 = 5 -> Void
notTrue Refl impossible

namespace WithMaybe

  checkEqNat : (n : Nat) -> (m : Nat) -> Maybe (n = m)
  checkEqNat Z Z = Just Refl
  checkEqNat Z (S k) = Nothing
  checkEqNat (S k) Z = Nothing
  checkEqNat (S k) (S j)
    = case checkEqNat k j of
        Just Refl => Just Refl
        Nothing => Nothing


  tryZip : Vect n a -> Vect m b -> Maybe (Vect n (a, b))
  tryZip {n} {m} xs ys
    = case checkEqNat n m of
        Just Refl => Just (zip xs ys)
        Nothing => Nothing

namespace WithDec -- Dec for decide

  zeroNotSucc : (0 = S k) -> Void
  zeroNotSucc Refl impossible

  succNotZero : (S k = 0) -> Void
  succNotZero Refl impossible

  succVoid : (contra : (k = j) -> Void) -> (S k = S j) -> Void
  succVoid contra Refl = contra Refl

  checkEqNat : (n : Nat) -> (m : Nat) -> Dec (n = m)
  checkEqNat Z Z = Yes Refl
  checkEqNat Z (S k) = No zeroNotSucc
  checkEqNat (S k) Z = No succNotZero
  checkEqNat (S k) (S j)
    = case checkEqNat k j of
        Yes Refl => Yes Refl
        No contra => No (succVoid contra)

  tryZip : Vect n a -> Vect m b -> Maybe (Vect n (a, b))
  tryZip {n} {m} xs ys
    = case checkEqNat n m of
        Yes Refl => Just (zip xs ys)
        No contra => Nothing
