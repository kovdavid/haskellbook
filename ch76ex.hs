module Chapter76Exercise where

functionC :: Ord a => a -> a -> a
functionC x y =
  case x > y of
    True  -> x
    False -> y


ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
  case even n of
    True  -> n + 2
    False -> n

nums :: (Num a, Num a1, Ord a) => a -> a1
nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1
