module Day1 where

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

t :: Tree String
t = Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c"))

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Branch l r) = Branch (treeMap f l) (treeMap f r)

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- Maybe Monad --
-- zipTree :: Tree a -> Tree b -> Maybe (Tree (a, b))
-- zipTree (Leaf a) (Leaf b) = Just (Leaf (a, b))

-- State Monad --
-- number :: Tree a -> Tree Int
-- number (Leaf a) s = (Leaf s) (s+1)
