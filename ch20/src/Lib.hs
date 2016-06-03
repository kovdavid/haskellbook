{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Foldable
import Data.Monoid

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

---------------------------------------------

data Optional a = Nada | Yep a

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- class Foldable (t :: * -> *) where
--   foldMap :: Monoid m => (a -> m) -> t a -> m
--   fold :: Monoid m => t m -> m

hole = undefined
data Hole = Hole

sum :: forall t a . (Foldable t, Num a) => t a -> a
sum xs = undefined
  where _ = xs :: (Foldable t, Num a) => t a
