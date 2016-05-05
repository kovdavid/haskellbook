module Ch10Warmup where

myCombine :: [String] -> [String] -> [(String, String, String)]
myCombine stops vowels = [ (x, y, z) | x <- stops,
                                       y <- vowels,
                                       z <- stops ]

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> (f x) || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs)
  | el == x = True
  | otherwise = myElem el xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' needle = foldr (\x acc -> x == needle || acc) False

myElem'' :: Eq a => a -> [a] -> Bool
myElem'' needle = any (needle ==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x == True then x:acc else acc) []
-- myFilter f = foldr (\x -> if f x == True then (x:) else id) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (id)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldl (\acc x -> if f acc x == GT then acc else x) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldl (\acc x -> if f acc x == LT then acc else x) (head xs) xs
