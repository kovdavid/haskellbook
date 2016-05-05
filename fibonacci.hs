module Fib where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
