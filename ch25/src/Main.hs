{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  -- (<*>) :: m (a -> b) -> m a -> m b
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- (Compose f) <*> (Compose a) = Compose $ (fmap (<*>) f) <*> a
  -- (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a
  (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: forall m a b. Applicative m => (a -> m b) -> Compose f g a -> m (Compose f g b)
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second f = bimap id f

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

  first f (Deux a c) = Deux (f a) c
  second g (Deux a b) = Deux a (g b)

data Const' a b = Const' a

instance Bifunctor Const' where
  bimap f _ (Const' x) = Const' (f x)

  first f (Const' x) = Const' (f x)
  second _ (Const' x) = Const' x

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

  first f (Drei x y z) = Drei x (f y) z
  second g (Drei x y z) = Drei x y (g z)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

  first f (SuperDrei x y) = SuperDrei x (f y)
  second _ (SuperDrei x y) = SuperDrei x y

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

  first _ (SemiDrei x) = SemiDrei x
  second _ (SemiDrei x) = SemiDrei x

data QC a b c d = QC a b c d

instance Bifunctor (QC a b) where
  bimap f g (QC a b c d) = QC a b (f c) (g d)

  first f (QC a b c d) = QC a b (f c) d
  second g (QC a b c d) = QC a b c (g d)

data EI a b = L a | R b

instance Bifunctor EI where
  bimap f _ (L x) = L (f x)
  bimap _ g (R x) = R (g x)

  first f (L x) = L (f x)
  first _ (R x) = R x

  second g (R x) = R (g x)
  second _ (L x) = L x

main :: IO ()
main = do
  putStrLn "hello world"
