module Ch77ex where

dodgy :: Num a => a -> a -> a
dodgy x y = x + y*10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = flip dodgy 2

myAbs :: Integer -> Integer
myAbs x
  | x < 0     = -x
  | otherwise = x

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.6 = 'D'
  | y <  0.6 = 'F'
  where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise        = False

numbers :: (Num a1, Ord a1, Num a) => a1 -> a
numbers x
  | x <  0 = -1
  | x == 0 = 0
  | x >  0 = 1
