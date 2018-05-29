module Day1

data Tree : a -> Type where
  Leaf : a -> Tree a
  Branch : Tree a -> Tree a -> Tree a

TREE : Tree String
TREE = Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c"))

Functor Tree where
  -- map : (f : a -> b) -> Tree a -> Tree b
  map f (Leaf a) = Leaf (f a)
  map f (Branch l r) = Branch (map f l) (map f r)

-- record State (s : Type) (a : Type) where
--   constructor MkState
--   g : (s -> (a, s))

data State : (s : Type) -> (a : Type) -> Type where
  MkState : (g : (s -> (a, s))) -> State s a

Functor (State s) where
  -- map : (f : a -> b) -> State s a -> State s b
  map f (MkState g) =
    MkState (\ s =>
      let (a, s') = g s
      in ((f a), s'))

Functor (State s) =>
Applicative (State s) where
  -- (<*>) : State s (a -> b) -> State s a -> State s b
  (MkState f) <*> (MkState g) =
    MkState (\ s =>
      let (f', s') = f s
          (a, s'') = g s'
      in (f' a, s''))
  -- pure : a -> State s a
  pure a = MkState (\ s => (a, s))

Applicative (State s) =>
Monad (State s) where
  -- (>>=) : State s a -> (a -> State s b) -> State s b
  (MkState g) >>= m =
    MkState (\ s =>
      let (a, s') = g s
          MkState f = m a
      in f s')

numberWithInt : Tree a -> (s : Int) -> (Tree Int, Int)
numberWithInt (Leaf _) s = (Leaf s, s+1)
numberWithInt (Branch l r) s =
  let (l', s') = numberWithInt l s
      (r', s'') = numberWithInt r s'
  in (Branch l' r', s'')

tick : State Int Int
tick = MkState (\s => (s, s+1))

number : Tree a -> State Int (Tree Int)
number (Leaf _) = do
  s <- tick
  pure (Leaf s)
number (Branch l r) = do
  l' <- number l
  r' <- number r
  pure (Branch l' r')
