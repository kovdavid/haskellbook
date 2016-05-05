module Main where

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

main :: IO ()
main = do
  putStr "Enter shift: "
  shiftStr <- getLine

  putStr "Enter word: "
  word <- getLine

  let shift = (read shiftStr) :: Int
   in putStrLn $ caesar shift word

  return ()
