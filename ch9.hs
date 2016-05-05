module Ch9 where

import Data.List (intersperse)

split' :: Char -> String -> [String]
split' splitBy []  = []
split' splitBy str = token : (split' splitBy $ dropWhile (== splitBy) rest)
  where token = takeWhile (/= splitBy) str
        rest  = dropWhile (/= splitBy) str

myfun :: String -> [String]
myfun str = split' ' ' str

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
myLines :: String -> [String]
myLines str = split' '\n' str

-- What we want 'myLines sentences' to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]
-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = print $ "Are they equal? "
               ++ show (myLines sentences == shouldEqual)

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

zipW' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipW' _ _ [] = []
zipW' _ [] _ = []
zipW' f (x:xs) (y:ys) = (f x y) : zipW' f xs ys

zip'' xs ys = zipW' (\x y -> (x, y)) xs ys
