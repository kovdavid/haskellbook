module ZipList where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
-- import Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList []
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

-- ========== List ========= --

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- (:)  5 []
-- Cons 5 Nil
--
-- ((:)  x xs)
-- (Cons x xs)
--
-- (++) []     ys = ys
-- (++) (x:xs) ys = x : xs ++ ys

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-- instance Applicative [] where
  -- pure x = (:) x []
  -- (<*>) [] _ = []
  -- (<*>) _ [] = []
  -- (<*>) ((:) x xs) ys = (x <$> ys) ++ (xs <*> ys)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = (f <$> xs) `append` (fs <*> xs)

take' :: Int -> List a -> List a
take' n xs = go n xs Nil
  where go 0 _ acc = acc
        go _ Nil acc = acc
        go n' (Cons x rest) acc = go (n'-1) rest (Cons x acc)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

-- ========== ZipList' ========= --

newtype ZipList' a = ZipList' (List a)
                     deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ Cons x Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) (ZipList' fs) (ZipList' xs) =
    ZipList' $ zip' fs xs
    -- fappend val (ZipList' fs <*> ZipList' xs)
      -- where fappend = fmap . append
            -- val = pure $ f x

zip' :: List (a -> b) -> List a -> List b
zip' Nil _ = Nil
zip' _ Nil = Nil
zip' (Cons f fs) (Cons x xs) = Cons (f x) (zip' fs xs)

functions = ZipList' $ Cons (+1) (Cons (+2) (Cons (+3) Nil))
values = ZipList' $ Cons 1 (Cons 2 (Cons 3 Nil))
