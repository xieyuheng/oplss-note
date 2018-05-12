module WithVar

Context : Type
Context = List Type

data ExpType : Type where
  NatType  : ExpType
  BoolType : ExpType

data Exp : ExpType -> Type where
  LitExp
    : (n : Nat) -> Exp NatType
  TrueExp
    : Exp BoolType
  FalseExp
    : Exp BoolType
  LessExp
    : (a, b : Exp NatType) -> Exp BoolType
  PlusExp
    : (a, b : Exp NatType) -> Exp NatType
  IfExp
    : {t : ExpType} ->
      (q : Exp BoolType) -> (a, e : Exp t) -> Exp t

Value : ExpType -> Type
Value NatType = Nat
Value BoolType = Bool

eval : {t : ExpType} -> Exp t -> Value t
eval (LitExp n) = n
eval TrueExp = False
eval FalseExp = False
eval (LessExp a b) = eval a < eval b
eval (PlusExp a b) = eval a + eval b
eval (IfExp q a e) = if eval q then eval a else eval e
