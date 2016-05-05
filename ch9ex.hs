module Ch9Ex where

import Data.Char

onlyUpper :: [Char] -> [Char]
onlyUpper = filter isUpper

capitalize :: [Char] -> [Char]
capitalize []     = []
capitalize (x:xs) = (toUpper x) : xs

cap' :: [Char] -> [Char]
cap' []     = []
cap' (x:xs) = (toUpper x) : cap' xs

capHead :: [Char] -> Char
capHead = toUpper . head

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
  | x == True  = and' xs
  | otherwise  = False

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' target (x:xs)
  | target == x = True
  | otherwise   = elem' target xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' _ [] = []
concatMap' f (x:xs) = (f x) ++ concatMap' f xs

concat2' :: [[a]] -> [a]
concat2' xs = concatMap' id xs

maxBy' :: (a -> a -> Ordering) -> [a] -> Maybe a
maxBy' _ [] = Nothing
maxBy' _ [x] = Just x
maxBy' f (x:xs) = go f x xs
  where go _ acc [] = Just acc
        go g acc (y:ys)
          | g acc y == LT = go g y ys
          | otherwise     = go g acc ys

minBy' :: (a -> a -> Ordering) -> [a] -> Maybe a
minBy' _ []     = Nothing
minBy' _ [x]    = Just x
minBy' f (x:xs) = go f x xs
  where go _ acc [] = Just acc
        go g acc (y:ys)
          | g y acc == LT = go g y ys
          | otherwise     = go g acc ys

max' :: Ord a => [a] -> Maybe a
max' xs = maxBy' compare xs
