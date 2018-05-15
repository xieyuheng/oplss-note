module Day2.Lambda

import Preface

data TermType : Type where
  NatType
    : TermType
  ArrowType
    : (succ, ante : TermType) ->
      TermType

Name : Type
Name = String

data Expr : Type where
  VarExpr
    : (x : Name) -> Expr
  LitExpr
    : Nat -> Expr
  SuccExpr
    : Expr
  ApplyExpr
    : Expr -> Expr -> Expr
  LambdaExpr
    : (x : Name) -> (a : TermType) -> Expr -> Expr

Context : Type
Context = List (Name, TermType)

data Term : (ctx : Context) -> TermType -> Type where
  VarTerm
    : (x : Name) ->
      (i : Member (x, a) ctx) ->
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

TypeError : Type
TypeError = String

TC : Type -> Type
TC t = Either TypeError t

typeError : TypeError -> TC t
typeError err = Left err

forgetType : (Term ctx a) -> Expr
forgetType (VarTerm x i) = VarExpr x
forgetType (LitTerm k) = LitExpr k
forgetType SuccTerm = SuccExpr
forgetType (ApplyTerm x y) =
  ApplyExpr (forgetType x) (forgetType y)
forgetType (LambdaTerm x a y) =
  LambdaExpr x a (forgetType y)

data Checked : (ctx : Context) -> Expr -> Type where
  OkChecked
    : (a : TermType) -> (v : Term ctx a) ->
      Checked ctx (forgetType v)

typeCheck : (ctx : Context) -> (e : Expr) -> TC (Checked ctx e)
typeCheck ctx (VarExpr x) =
  ?typeCheck_rhs_1
typeCheck ctx (LitExpr k) =
  Right (OkChecked NatType (LitTerm k))
typeCheck ctx SuccExpr =
  Right (OkChecked (ArrowType NatType NatType) (SuccTerm))
typeCheck ctx (ApplyExpr x y) =
  ?typeCheck_rhs_4
typeCheck ctx (LambdaExpr x a y) =
  ?typeCheck_rhs_5
