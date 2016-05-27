module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _    = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _  = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = elements [NopeDotJpg]

instance EqProp (Nope a) where
  (=-=) = eq

---------------------------------------------------------

data PEuther b a =
    Le a
  | Ri b
  deriving (Eq, Show)

instance Functor (PEuther b) where
  fmap _ (Ri x) = Ri x
  fmap f (Le x) = Le $ f x

instance Applicative (PEuther b) where
  pure = Le
  (<*>) (Le f) (Le x) = Le (f x)
  (<*>) (Ri x) _ = Ri x
  (<*>) _ (Ri x) = Ri x

instance Monad (PEuther b) where
  return = pure
  (>>=) (Le x) f = f x
  (>>=) (Ri x) _ = Ri x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PEuther b a) where
  arbitrary = oneof [ Le <$> arbitrary
                    , Ri <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (PEuther b a) where
  (=-=) = eq

---------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

---------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil
  (<*>) (Cons f fs) xs = (f <$> xs) `append` (fs <*> xs)
  (<*>) Nil _ = Nil

instance Monad List where
  return = pure
  (>>=) (Cons x xs) f = (f x) `append` (xs >>= f)
  (>>=) Nil _ = Nil

instance Foldable List where
  foldr _ acc Nil = acc
  foldr f acc (Cons x xs) = foldr f (f x acc) xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = return <$> arbitrary
  -- arbitrary = sized $ \n ->
    -- do k <- choose (0,n)
       -- sequence [ arbitrary | _ <- [1..k] ]


instance Eq a => EqProp (List a) where
  (=-=) = eq

-- instance Applicative [] where
  -- pure x = [x]
  -- (<*>) (f:fs) xs = (f <$> xs) ++ (fs <*> xs)

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = liftM2 (:) (f x) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id

main :: IO ()
main = do
  -- quickBatch $ monad (undefined :: Nope (String, Int, String))
  -- quickBatch $ monad (undefined :: PEuther Int (String, Int, String))
  -- quickBatch $ monad (undefined :: Identity (String, Int, String))
  quickBatch $ monad (undefined :: List (String, Int, String))
