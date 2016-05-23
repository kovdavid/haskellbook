module ChapterExercises where

import Data.Monoid
import Control.Applicative (liftA3)

newtype Identity a = Identity a deriving Show

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

------

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

------

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two e f) (Two g x) = Two (e <> g) (f x)

------

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three f g h) (Three x y z) = Three (f <> x) (g <> y) (h z)

------

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' f g h) (Three' x y z) = Three' (f <> x) (g y) (h z)

------

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four f g h i) (Four u v w x) = Four (f <> u) (g <> v) (h <> w) (i x)

------

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' a1 b1 c1 d1) (Four' a2 b2 c2 d2) =
    Four' (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 d2)

------

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos xs ys zs = liftA3 (,,) xs ys zs

main :: IO ()
main = return ()
