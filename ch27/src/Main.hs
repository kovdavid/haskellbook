{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Monad
import Control.Applicative
import Debug.Trace

data Test
  = A Test2
  | B Test2
  deriving (Show)

data Test2
  = C Int
  | D Int
  deriving (Show)

forceNothing :: Test -> Int
forceNothing _ = 0

forceTest :: Test -> Int
forceTest (A _) = 1
forceTest (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C i)) = i
forceTest2 (A (D i)) = i
forceTest2 (B (C i)) = i
forceTest2 (B (D i)) = i

inc = (+ 1)

twice = inc . inc

howManyTimes =
  inc (trace "I got eval'd" (1 + 1)) + twice (trace "I got eval'd" (1 + 1))

howManyTimes' =
  let onePlusOne = trace "I got eval'd" (1 + 1)
  in inc onePlusOne + twice onePlusOne

main :: IO ()
main = do
  return ()
