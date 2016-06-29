{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative
import Data.Char

main :: IO ()
main = return ()

hurr :: Integer -> Integer
hurr = (*2)

durr :: Integer -> Integer
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

-- Short Exercise: Warming Up

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

monadic :: [Char] -> ([Char], [Char])
monadic = do
  x <- cap
  y <- rev
  return (x, y)

monadic' :: [Char] -> ([Char], [Char])
monadic' =
  cap >>= \x ->
    rev >>= \y ->
      return (x,y)

-- >>= :: m a -> (a -> m b) -> m b
-- m   :: ((->) [Char])
-- cap :: ((->) [Char]) [Char]   == m [Char]
-- rev :: ((->) [Char]) [Char]   == m [Char]
-- (,) :: [Char] -> [Char] -> ([Char], [Char])
--     :: ((->) [Char]) ((->) [Char]) ([Char], [Char])
--     :: m (m ([Char], [Char]))
--
--
-- instance Functor ((->) r) where
--   fmap = (.)

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id


newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris")
         (DogName "Papu")
         (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' =
  liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 x y z = x <$> y <*> z

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

-- newtype Reader r a = Reader { runReader :: r -> a }

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  -- (Reader ra) >>= aRB = Reader $ \r -> runReader (aRB (ra r)) r
  x >>= f = Reader $ \r -> runReader (f (runReader x r)) r

getDogR'' :: Reader Person Dog
getDogR'' = Reader getDog

-------- Function Monads

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty r = bar (foo r) r

-- frooty' = \r -> bar (foo r) r

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Person -> Dog
getDogRM' =
  dogName >>= \name ->
    address >>= \address ->
      return $ Dog name address

getDogRM'' :: Person -> Dog
getDogRM'' =
  runReader $
    Reader dogName >>= \name ->
      Reader address >>= \address ->
        return $ Dog name address
        -- return $ Dog name address
