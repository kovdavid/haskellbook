module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b
