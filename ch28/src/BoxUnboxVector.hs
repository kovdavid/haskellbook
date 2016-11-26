{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

testData :: [Int]
testData = [1..9999]

boxedVector :: [Int] -> V.Vector Int
boxedVector t = V.fromList t

unboxedVector :: [Int] -> VU.Vector Int
unboxedVector t = VU.fromList t

main :: IO ()
main = defaultMain
  [ bench "Unboxed"
    $ nf unboxedVector testData
  , bench "Boxed"
    $ nf boxedVector testData
  ]
