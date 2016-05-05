module Main where

-- import Test.Hspec
import Test.QuickCheck

half :: (Fractional a, Num a) => a -> a
half x = x / 2

halfIdentity :: (Fractional a, Num a) => a -> a
halfIdentity = (*2) . half

propHalf :: Property
propHalf =
  forAll (arbitrary :: Gen Double)
  (\d -> halfIdentity d == d)

main :: IO ()
main = quickCheck propHalf
