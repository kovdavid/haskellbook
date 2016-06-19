module TraverseTree where

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node t1 x t2) = Node (fmap f t1) (f x) (fmap f t2)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node t1 x t2) = (foldMap f t1) `mappend` (f x) `mappend` (foldMap f t2)

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node t1 x t2) = Node <$> (traverse f t1) <*> (f x) <*> (traverse f t2)
