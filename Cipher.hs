module Main where

import Test.QuickCheck
import Test.QuickCheck.Instances.Char (lowerAlpha)
import Data.Char

ordA :: Int
ordA = ord 'a'
ordZ :: Int
ordZ = ord 'z'

keepaz :: Int -> Int
keepaz num
  | num <  ordA = num + ordZ - ordA + 1
  | num <= ordZ = num
  | num >  ordZ = num - ordZ + ordA - 1

caesar :: Int -> String -> String
caesar shift str = map (chr . keepaz . (+shift) . ord) str

unCaesar :: Int -> String -> String
unCaesar shift str = caesar (negate shift) str

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString = listOf genSafeChar

propCaesar :: Property
propCaesar =
  forAll genSafeString
  (\str -> (caesar 5 . unCaesar 5 $ str) == str)

main :: IO ()
main = do
  quickCheck propCaesar

-- main :: IO ()
-- main = do
  -- putStr "Enter shift: "
  -- shiftStr <- getLine

  -- putStr "Enter word: "
  -- word <- getLine

  -- let shift = (read shiftStr) :: Int
   -- in putStrLn $ caesar shift word

  -- return ()
