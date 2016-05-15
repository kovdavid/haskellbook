{-# LANGUAGE FlexibleInstances #-}

module ChapterEx where

data Sum a b = First b | Second a

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

data Company a c b = DeepBlue a c | Somthing b

instance Functor (Company e e') where
  fmap f (Somthing b) = Somthing (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Bloor x) = Bloor $ f x

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip $ K' (f x)

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

----------------------------------------

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut $ g <$> x

----------------------------------------

data Parappa f g a = DaWrape (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrape x y) = DaWrape (f <$> x) (f <$> y)

----------------------------------------

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (f <$> y)

----------------------------------------

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (f <$> z)

----------------------------------------

data List a = Nil | Cons a (List a) deriving (Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x rest) = Cons (f x) (f <$> rest)

----------------------------------------

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor (GoatLord) where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (f <$> x) (f <$> y) (f <$> z)

----------------------------------------

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g) = Read $ f <$> g

main :: IO ()
main = return ()
