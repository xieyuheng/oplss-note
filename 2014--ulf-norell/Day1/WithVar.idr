module Day1.WithVar

import Preface

-- todo --
-- - give some examples for each relation.
--   require more the reasoned schemer

data ExpType : Type where
  NatType  : ExpType
  BoolType : ExpType

Context : Type
Context = List ExpType

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

-- when writing this,
--   view `All p xs` as a `List t`,
--   and `Member x xs` as `Nat`.
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

-- generalize `Member` by `Any` --

MemberByAny : (x : t) -> List t -> Type
MemberByAny x xs = Any ((=) x) xs

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

export
test : IO ()
test = assert (VALUE == 9)
