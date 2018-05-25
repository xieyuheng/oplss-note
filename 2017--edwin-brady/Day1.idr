module Day1

StringOrNat : (isStr : Bool) -> Type
StringOrNat False = Nat
StringOrNat True = String

lengthOrDouble : (isStr : Bool) -> (StringOrNat isStr) -> Nat
lengthOrDouble False x = x + x
lengthOrDouble True x = length x

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

(+) : Nat -> Nat -> Nat
Z + j = j
(S k) + j = S (k + j)

%name Vect xs, ys, zs

append : Vect n a -> Vect m a -> Vect (n + m) a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

-- append' : Vect n a -> Vect m a -> Vect (n + m) a
-- append' [] ys = [] -- instead of `ys`
-- append' (x :: xs) ys = x :: append' xs ys

--   When checking right hand side of append' with expected type
--           Vect (0 + m) a
--   Type mismatch between
--           Vect 0 a (Type of [])
--           Vect m a (Expected type)

-- when type checking antecedent
--   an unification between Vect 0 a (type of []) and Vect n a
--     constrains n to 0

-- when type checking succedent
--   returned type and expected type must be the same
--     no unification should be done here
--   type of input argument should not be constrained by
--     the type of a return value

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

emptyMatrix : Vect m (Vect 0 t)
emptyMatrix {m = Z} = []
emptyMatrix {m = (S k)} = [] :: emptyMatrix

-- we must be able to pull data from type
--   to be able to write `emptyMatrix`

-- thus, runtime must contain data in type in almost all cases,
--   which makes the language dynamicly typed at runtime
--   with even more overhead

transposeMatrixHelper : Vect m t -> Vect m (Vect k t) -> Vect m (Vect (S k) t)
transposeMatrixHelper [] [] = []
transposeMatrixHelper (x :: xs) (y :: ys)
  = (x :: y) :: transposeMatrixHelper xs ys

transposeMatrix : Vect n (Vect m t) -> Vect m (Vect n t)
transposeMatrix [] = emptyMatrix
transposeMatrix (x :: xs)
  = let xs_trans = transposeMatrix xs in
      transposeMatrixHelper x xs_trans

-- test in repl --
-- transposeMatrix [[1, 2, 3],
--                  [4, 5, 6],
--                  [7, 8, 9]]
-- [[1, 4, 7],
--  [2, 5, 8],
--  [3, 6, 9]] : Vect 3 (Vect 3 Integer)
