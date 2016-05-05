{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Ch11TooMany where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n == 2

instance TooMany (Int, String) where
  tooMany (x, y) = tooMany (x + (length y))

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 10

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)
