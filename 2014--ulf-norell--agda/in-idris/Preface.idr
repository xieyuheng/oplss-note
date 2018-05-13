module Preface

public export
data Member : (x : t) -> List t -> Type where
  ZeroMember : Member x (x :: xs)
  SuccMember : Member x xs -> Member x (p :: xs)

public export
data All : (p : t -> Type) -> List t -> Type where
  NullAll : All p []
  ConsAll : (p x) -> All p xs -> All p (x :: xs)

public export
data Any : (p : t -> Type) -> List t -> Type where
  ZeroAny : (p x) -> Any p (x :: xs)
  SuccAny : Any p xs -> Any p (x :: xs)

export
assert : Bool -> IO ()
assert b =
  if b
  then putStr "."
  else putStr "X"

export
test : IO ()
test = do
  assert (1 == 1)
