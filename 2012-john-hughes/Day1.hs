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

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

zipTree :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree (Leaf x) (Leaf y) = Just (Leaf (x, y))
zipTree (Branch l r) (Branch l' r') =
  liftA2 Branch (zipTree l l') (zipTree r r')
zipTree _ _ = Nothing

-- data State a = ??? -- to use record type

-- number :: Tree a -> State Int (Tree Int)
-- number (Leaf _) = liftM Leaf tick
-- number (Branch l r) = liftM2 Branch (number l) (number r)
