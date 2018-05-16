||| - dependent type system as logic programming language
|||
|||   - a [typical] dependent type syetem
|||     in lack of reification and query interfaces
|||
|||   - a [typical] logic programming
|||     in lack of the informations
|||     about the names of inference rules

--- in prolog ---
-- append([], SUCC, SUCC).
-- append([CAR|CDR], SUCC, [CAR|RESULT_CDR]):-
--   append(CDR, SUCC, RESULT_CDR).

data Append : (ante, succ, result : List t) -> Type where
  ZeroAppend
    : Append [] succ result
  SuccAppend
    : (prev : Append cdr succ result_cdr) ->
      Append (car :: cdr) succ (car :: result_cdr)

APPEND1 : Append [] [2] [2]
APPEND1 = ZeroAppend

APPEND2 : Append [1] [2] [1, 2]
APPEND2 = SuccAppend ZeroAppend

APPEND3 : Append [1, 2] [3, 4] [1, 2, 3, 4]
APPEND3 = SuccAppend (SuccAppend ZeroAppend)
