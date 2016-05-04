module Tmp where

fibs = 1 : scanl (+) 1 fibs

fibs = 1 : scanl + 1 fibs
fibs = 1 : (1 : scanl + (+ 1 $ head (1:_))) (tail (1:_))
fibs = 1 : 1 : (scanl + (2) (1:_))
fibs = 1 : 1 : (2 : scanl + (+ 2 $ head (1:_)) (tail 1:_))
fibs = 1 : 1 : 2 : (scanl + 3 (2:_))
