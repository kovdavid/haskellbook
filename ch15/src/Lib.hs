module Lib
    ( someFunc
    ) where

import Test.QuickCheck
import MadLibs hiding (check)

-- algebra: operations + set they operate over (type)
-- <> -> infix mappend

-- First (Just 1) <> First (Just 2) === First {getFirst = Just 1}

data Booly a =
    False'
  | True'
  deriving (Eq, Show)

-- instance Monoid (Booly a) where
  -- mappend False' _    = False'
  -- mappend _ False'    = False'
  -- mappend True' True' = True'

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only $ mappend x y
  mappend (Only x) Nada = Only x
  mappend Nada (Only x) = Only x
  mappend Nada Nada = Nada

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype First' a =
  First' (Optional a)
  -- First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    arb <- (arbitrary :: Arbitrary a => Gen a)
    val <- elements [Only arb, Nada]
    return (First' val)

instance Monoid (First' a) where
  mempty = First' Nada

  mappend (First' (Only x)) _ = First' $ Only x
  mappend _ (First' (Only x)) = First' $ Only x
  mappend _ _                 = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend (First' (Only x)) _ = First' $ Only x
firstMappend _ (First' (Only x)) = First' $ Only x
firstMappend _ _                 = First' Nada

type FirstMappend =
  First' String
  -> First' String
  -> First' String
  -> Bool

check :: IO ()
check = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
