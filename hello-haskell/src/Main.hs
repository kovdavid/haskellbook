module Main where

import System.IO

import Hello

main :: IO String
main = do
  x1 <- getLine
  x2 <- getLine
  return (x1 ++ x2)
  -- hSetBuffering stdout NoBuffering
  -- putStr "Input name: "
  -- name <- getLine
  -- sayHello name
