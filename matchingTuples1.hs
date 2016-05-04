module TupleFunctions where

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
