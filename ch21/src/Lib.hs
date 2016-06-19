{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Data.Foldable
import Data.Monoid

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--------------------------------------------------------

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

--------------------------------------------------------

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [ return Nada
                    , Yep <$> arbitrary
                    ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

--------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = (f x) `mappend` (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = (flip Cons) Nil <$> arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq

--------------------------------------------------------

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

--------------------------------------------------------

data Three' a b = Three' a b b deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Foldable (Three' a) where
  foldMap f (Three' _ y z) = f y `mappend` f z

instance Traversable (Three' a) where
  traverse f (Three' x y z) = Three' x <$> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

--------------------------------------------------------

data S n a = S (n a) a deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (f <$> x) (f y)

instance (Functor n, Foldable n) => Foldable (S n) where
  foldMap f (S x y) = fold (f <$> x) `mappend` (f y)

instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (traversable $ (undefined :: Identity (Int, Int, [Int])))
  quickBatch (traversable $ (undefined :: Constant Int (Int, Int, [Int])))
  quickBatch (traversable $ (undefined :: Optional (Int, Int, [Int])))
  quickBatch (traversable $ (undefined :: List (Int, Int, [Int])))
  quickBatch (traversable $ (undefined :: Three Char String (Int, Int, [Int])))
  quickBatch (traversable $ (undefined :: Three' Int (Int, Int, [Int])))
  quickBatch (traversable $ (undefined :: S Maybe ([Int], [Int], [Int])))
  return ()
