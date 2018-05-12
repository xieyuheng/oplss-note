module WithVar

-- todo
-- - give some examples for each relation.

data ExpType : Type where
  NatType  : ExpType
  BoolType : ExpType

Context : Type
Context = List ExpType

data Member : (x : t) -> List t -> Type where
  ZeroMember : Member x (x :: xs)
  SuccMember : Member x xs -> Member x (y :: xs)

data Exp : (ctx : Context) -> ExpType -> Type where
  VarExp
    : Member t ctx -> Exp ctx t
  LitExp
    : (n : Nat) -> Exp ctx NatType
  TrueExp
    : Exp ctx BoolType
  FalseExp
    : Exp ctx BoolType
  LessExp
    : (a, b : Exp ctx NatType) ->
      Exp ctx BoolType
  PlusExp
    : (a, b : Exp ctx NatType) ->
      Exp ctx NatType
  IfExp
    : (q : Exp ctx BoolType) ->
      (a, e : Exp ctx t) -> Exp ctx t

Value : ExpType -> Type
Value NatType = Nat
Value BoolType = Bool

data All : (p : t -> Type) -> List t -> Type where
  NullAll : All p []
  ConsAll : (p x) -> All p xs -> All p (x :: xs)

||| when writing this
|||   view `All p xs` as a `List t`
|||   and `Member x xs` as `Nat`
loopUpMember : All p xs -> Member x xs -> (p x)
loopUpMember (ConsAll h hs) ZeroMember = h
loopUpMember (ConsAll h hs) (SuccMember prev) =
  loopUpMember hs prev

Env : Context -> Type
Env ctx = All Value ctx

eval : (env : Env ctx) -> Exp ctx t -> Value t
eval env (VarExp member_h) = loopUpMember env member_h
eval env (LitExp n) = n
eval env TrueExp = False
eval env FalseExp = False
eval env (LessExp a b) = eval env a < eval env b
eval env (PlusExp a b) = eval env a + eval env b
eval env (IfExp q a e) =
  if eval env q
  then eval env a
  else eval env e

-- test --

CTX : Context
CTX = [NatType, BoolType]

ENV : All Value CTX
ENV = ConsAll 5 (ConsAll False NullAll)

EXP : Exp CTX NatType
EXP = IfExp
        (VarExp (SuccMember ZeroMember))
        (VarExp ZeroMember)
        (PlusExp (LitExp 4) (VarExp ZeroMember))

VALUE : Nat
VALUE = eval ENV EXP

-- VALUE == 9

-- generalize `Member` by `Any`

data Any : (p : t -> Type) -> List t -> Type where
  ZeroAny : (p x) -> Any p (x :: xs)
  SuccAny : Any p xs -> Any p (x :: xs)

MemberByAny : (x : t) -> List t -> Type
MemberByAny x xs = Any ((=) x) xs
