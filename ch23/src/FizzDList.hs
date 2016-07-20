{-# LANGUAGE InstanceSigs #-}
module FizzDList where

import Control.Monad (replicateM)
import Control.Monad.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list =
  execState (mapM_ addResult list) DL.empty

fizzBuzzFromTo :: Integer -> Integer -> DL.DList String
fizzBuzzFromTo upper lower =
  fizzBuzzList $ [upper, (upper-1) .. lower]

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main :: IO ()
main = do
  mapM_ putStrLn $ fizzBuzzFromTo 30 20
