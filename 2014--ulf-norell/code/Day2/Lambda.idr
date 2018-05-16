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

-- not only informations in the type are forgotten
-- in the case of `VarTerm`
--   the informations in the data is also forgotten
forgetType : (Term ctx a) -> Expr
forgetType (VarTerm x i) = VarExpr x
forgetType (LitTerm k) = LitExpr k
forgetType SuccTerm = SuccExpr
forgetType (ApplyTerm x e) =
  ApplyExpr (forgetType x) (forgetType e)
forgetType (LambdaTerm x a e) =
  LambdaExpr x a (forgetType e)

data Checked : (ctx : Context) -> Expr -> Type where
  OkChecked
    : (a : TermType) -> (v : Term ctx a) ->
      Checked ctx (forgetType v)

checkedLam
  : Checked ((x, a) :: ctx) e ->
    Checked ctx (LambdaExpr x a e)
checkedLam (OkChecked b v) =
  OkChecked (ArrowType _ b) (LambdaTerm _ _ v)

lookupVar
  : (ctx : Context) -> (x : Name) ->
    TC (A : TermType ** Member (x, A) ctx)
lookupVar ctx x = ?asdf

checkedVar
  : (A : TermType ** Member (x, A) ctx) ->
    Checked ctx (VarExpr x)
checkedVar (a ** i) = OkChecked a (VarTerm _ i)

typeCheck : (ctx : Context) -> (e : Expr) -> TC (Checked ctx e)
typeCheck ctx (VarExpr x) =
  map checkedVar (lookupVar ctx x)
typeCheck ctx (LitExpr k) =
  Right (OkChecked NatType (LitTerm k))
typeCheck ctx SuccExpr =
  Right (OkChecked (ArrowType NatType NatType) (SuccTerm))
typeCheck ctx (ApplyExpr x e) =
  ?typeCheck_rhs_4
typeCheck ctx (LambdaExpr x a e) =
  map checkedLam (typeCheck ((x, a) :: ctx) e)
