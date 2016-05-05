module Ch11Ex where

import Data.Char

ord_a = ord 'a'
ord_z = ord 'z'

vigenere :: String -> String -> String
vigenere shiftText text =
  let infShiftText = map shiftNum $ (concat . repeat) $ shiftText
  in  go infShiftText text
  where go _ [] = []
        go xs (' ':ys) = ' ' : go xs ys
        go (x:xs) (y:ys) = (shiftChar x y) : go xs ys

shiftNum :: Char -> Int
shiftNum c = (ord c) - ord_a

shiftChar :: Int -> Char -> Char
shiftChar s c = chr $ normalize $ s + ord c
  where normalize x
          | x < ord_a = x + ord_z - ord_a + 1
          | x > ord_z = x - ord_z + ord_a - 1
          | otherwise = x

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xxs@(x:xs) (y:ys)
  | x == y    = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf xxs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map go . words
  where go [] = ("", "")
        go xxs@(x:xs) = (xxs, (toUpper x) : xs)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph xs = capitalizeWord paragraph ++ capitalizeWord rest
  where (s,r) = break (== '.') xs
        paragraph = s ++ takeWhile (\c -> c == '.' || c == ' ') r
        rest = dropWhile (\c -> c == '.' || c == ' ') r
