module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

foolGen' :: Gen Fool
foolGen' = elements [Fulse, Fulse, Frue]

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> ((charToMorse c) >>= morseToChar) == Just c)

half x = x / 2

halfIdentity = (*2) . half

prop_half :: Property
prop_half =
  forAll (arbitrary :: Gen Double)
  (\c -> halfIdentity c == c)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_ordered :: Property
prop_ordered =
  forAll (arbitrary :: Gen [Int])
  (\xs -> (listOrdered $ sort xs) == True)

plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative x y =
  x + y == y + x

prop_plusAssociative :: Property
prop_plusAssociative =
  forAll (arbitrary :: Gen (Int, Int, Int))
  (\(x, y, z) -> plusAssociative x y z == True)

prop_plusCommutative =
  forAll (arbitrary :: Gen (Int, Int))
  (\(x, y) -> plusCommutative x y == True)

testQuot x y =
  (quot x y)*y + (rem x y) == x

testDiv x y =
  (div x y)*y + (mod x y) == x

prop_testQuot =
  forAll (arbitrary :: Gen (NonZero Int, NonZero Int))
  (\(NonZero x, NonZero y) -> testQuot x y == True)

prop_testDiv =
  forAll (arbitrary :: Gen (NonZero Int, NonZero Int))
  (\(NonZero x, NonZero y) -> testDiv x y == True)

associativePov x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

prop_testAssPov =
  forAll (arbitrary :: Gen (NonNegative Int, NonNegative Int, NonNegative Int))
  (\(NonNegative x, NonNegative y, NonNegative z) -> associativePov x y z == True)

test_doubleReverse xs =
  (reverse . reverse $ xs) == id xs

prop_reverse =
  forAll (arbitrary :: Gen [Char])
  (\xs -> test_doubleReverse xs == True)

prop_dollar =
  forAll (arbitrary :: Gen Int) go_test
  where f = (+2)
        go_test x = (f $ x) == f x

prop_dot =
  forAll (arbitrary :: Gen Int) go_test
  where f = (+2)
        g = (*3)
        go_test x = (f . g $ x) == f (g x)

f n xs = length (take n xs) == n

g x = (read (show x)) == x

twice f = f . f
fourTime = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x) : xs

main :: IO ()
main = do
  quickCheck prop_dollar
  -- quickCheck prop_dot

  -- quickCheck $
    -- forAll (arbitrary :: Gen (String, NonNegative Int))
           -- (\(xs, NonNegative n) -> f n xs == True)

  quickCheck $
    forAll (arbitrary :: Gen String)
           (\str -> let cw = capitalizeWord str
                     in cw == (twice capitalizeWord) str && cw == (fourTime capitalizeWord) str)

  quickCheck $
    forAll (arbitrary :: Gen String)
           (\str -> let cw = sort str
                     in cw == (twice sort) str && cw == (fourTime sort) str)

  -- quickCheck $
    -- forAll (arbitrary :: Gen Int)
           -- (\x -> g x == True)

  -- quickCheck $
    -- forAll (arbitrary :: Gen Double)
           -- (\x -> let sq n = n * n
                   -- in (sq . sqrt) x == x)

  -- quickCheck $
    -- forAll (arbitrary :: Gen ([String]))
           -- (\xs -> let f = foldr (++) []
                       -- g = concat
                   -- in f xs == g xs)

  -- quickCheck $
    -- forAll (arbitrary :: Gen (String, String))
           -- (\(xs, ys) -> let f = foldr (:)
                             -- g = (++)
                          -- in f xs ys == g xs ys)
                          
  -- quickCheck prop_plusAssociative
  -- quickCheck prop_plusCommutative
  -- quickCheck prop_testQuot
  -- quickCheck prop_testDiv

