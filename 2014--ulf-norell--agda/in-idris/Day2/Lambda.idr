module Day2.Lambda

import Preface

data TermType : Type where
  NatType : TermType
  ArrowType : (succ, ante : TermType) -> TermType

Name : Type
Name = String

Context : Type
Context = List (Name, TermType)

data Term : (ctx : Context) -> TermType -> Type where
  VarTerm
    : (x : Name) ->
      Member (x, a) ctx ->
      Term ctx a
  LitTerm
    : Nat ->
      Term ctx NatType
  SuccTerm
    : Term ctx (ArrowType NatType NatType)
  ApplyTerm
    : Term ctx (ArrowType a b) ->
      Term ctx a ->
      Term ctx b
  LambdaTerm
    : (x : Name) -> (a : TermType) ->
      Term ((x, a) :: ctx) b ->
      Term ctx (ArrowType a b)
