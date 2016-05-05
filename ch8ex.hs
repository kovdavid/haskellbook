module Ch8ex where

import Data.List (intersperse)

sumAll :: (Eq a, Num a) => a -> a
sumAll 1 = 1
sumAll n = n + sumAll(n - 1)

mull' :: (Integral a) => a -> a -> a
mull' x y = go x y 0
  where go x' y' acc
         | x' == 0 = acc
         | otherwise = go (x' - 1) y' (acc + y')

data DividedResult = Result Integer | DivideByZero deriving (Show)

(<~>) :: (Integer -> Integer) -> DividedResult -> DividedResult
(<~>) _ DivideByZero = DivideByZero
(<~>) f (Result num) = Result (f num)

dividedBy :: (Integral a) => a -> a -> DividedResult
dividedBy _   0     = DivideByZero
dividedBy num denom
  | num < 0   = negate <~> dividedBy (negate num) denom
  | denom < 0 = negate <~> dividedBy num (negate denom)
  | otherwise = go num denom 0
  where go n d count
         | n < d = Result count
         | otherwise = go (n - d) d (count + 1)

mc91 :: Integer -> Integer
mc91 x
  | x > 100 = x - 10
  | otherwise = 91

digitToWord :: Int -> String
digitToWord 1 = "One"
digitToWord 2 = "Two"
digitToWord 3 = "Three"
digitToWord 4 = "Four"
digitToWord 5 = "Five"
digitToWord 6 = "Six"
digitToWord 7 = "Seven"
digitToWord 8 = "Eight"
digitToWord 9 = "Nine"
digitToWord _ = "Zero"

digits :: Int -> [Int]
digits 0 = []
digits n = (digits $ div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n

eftBool :: Bool -> Bool -> [Bool]
eftBool True True   = [True]
eftBool True False  = []
eftBool False True  = [False, True]
eftBool False False = [False]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT LT = [LT]
eftOrd EQ EQ = [EQ]
eftOrd GT GT = [GT]
eftOrd _  _  = []

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x == y = [x]
  | x > y = []
  | x < y = [x] ++ (eftInt (x + 1) y)

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x == y = [x]
  | x > y = []
  | x < y = [x] ++ (eftChar (succ x) y)
