module Lib where

import Data.List (elemIndex)
import Data.Monoid ((<>))

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1..5]

x' :: Maybe Int
x' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> x'

xs = [1, 2, 3]
ys = [4, 5, 6]

xx :: Maybe Integer
xx = lookup 3 $ zip xs ys

yy :: Maybe Integer
yy = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> xx <*> yy

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity v) = Identity (f v)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity v) = Identity (f v)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant v) = Constant v

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant v) (Constant w) = Constant $ v <> w

data Cow = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  Cow <$> noEmpty name'
      <*> noNegative age'
      <*> noNegative weight'
