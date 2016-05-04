module CH8Ex where

dividedBy num denum = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

dividedBy 15 2
