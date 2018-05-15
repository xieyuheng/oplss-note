-- append([], SUCC, SUCC).
-- append([CAR|CDR], SUCC, [CAR|RESULT_CDR]):-
--   append(CDR, SUCC, RESULT_CDR).

data Append : (ante, succ, result : List t) -> Type where
  ZeroAppend
    : Append [] succ result
  SuccAppend
    : (prev : Append cdr succ result_cdr) ->
      Append (car :: cdr) succ (car :: result_cdr)

APPEND : Append [] [2] [2]
APPEND = ZeroAppend

APPEND2 : Append [1] [2] [1, 2]
APPEND2 = SuccAppend ZeroAppend
