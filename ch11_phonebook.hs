module Main where

import Data.Char
import Data.List
import Data.Ord

-- 2 ABC2
-- 3 DEF3
-- 4 GHI4
-- 5 JKL5
-- 6 MNO6
-- 7 PQRS7
-- 8 TUV8
-- 9 WXYZ9
-- * ^* (uppercase)
-- 0 +_0
-- # .,#

data DaPhone = DaPhone [(Char, String)]

phone :: DaPhone
phone =
  DaPhone [ ('1', "1"),
            ('2', "abc2"),
            ('3', "def3"),
            ('4', "ghi4"),
            ('5', "jkl5"),
            ('6', "mno6"),
            ('7', "pqrs7"),
            ('8', "tuv8"),
            ('9', "wxyz9"),
            ('*', "^*"),
            ('0', "+ 0"),
            ('#', ".,#") ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' needle haystack = go haystack 0
  where go [] _ = 0
        go (x:xs) i
          | x == needle = i
          | otherwise   = go xs (i+1)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps daPhone@(DaPhone p) c
  | isUpper c = (reverseTaps daPhone '^') ++ (reverseTaps daPhone (toLower c))
  | otherwise =
    let buttons = filter filterButton p
        filterButton (_, chars) = elem c chars
        buttonPresses (digit, digitChars) = (digit, (+1) $ elemIndex' c digitChars)
     in map buttonPresses buttons

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p str = concat $ map (reverseTaps p) str

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_,p) acc -> p + acc) 0

mostPopularLetter :: Ord a => [a] -> a
mostPopularLetter str = head $ mostUsedLetters str

mostUsedLetters str = head $ reverse $ sortBy (comparing length) $ group $ sort str

-- letterCost c = foldr (\(_, cost) acc -> acc + cost) 0 $ reverseTaps phone c
letterCost c = fingerTaps . (reverseTaps phone) $ c

mostUsedLetterCost str =
  let letters = mostUsedLetters str
      cost = letterCost $ head letters
   in length letters * cost

coolestLtr :: [String] -> Char
coolestLtr strs =
  let str = filter (/= ' ') $ concat strs
   in mostPopularLetter str

coolestWord :: [String] -> String
coolestWord strings =
  let words' = words $ concat strings
   in mostPopularLetter words'

main :: IO ()
main = do
  print $ show $ reverseTaps phone 'A'
  print $ show $ cellPhonesDead phone "Hello World1"
  print $ show $ fingerTaps $ cellPhonesDead phone "Hello World1"
