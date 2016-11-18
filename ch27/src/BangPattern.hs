{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
module BangPattern where

import Control.Monad
import Control.Applicative

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

banging :: Bool -> Int
banging !b = 1

data Foo = Foo Int !Int

first (Foo x _) = x
second (Foo _ y) = y

main :: IO ()
main = do
  return ()
