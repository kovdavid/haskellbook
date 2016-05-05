module Ch10 where

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f acc []    = acc
-- foldr f acc (x:xs = f x (foldr f acc xs)

-- foldl :: (b -> a -> b) -> b -> [a] -> b

foldl (flip (*)) 5 [1..3]
foldl (flip (*)) 5 (1 : 2 : 3 : [])

((5 * 1) * 2) * 3

((5 `f*` 1) `f*` 2) `f*` 3

let x = flip (*)

(x (x (x 5 1) 2) 3)

let x = const

1 : 2 : 3 : 4 : 5 : []
(1 x (2 x (3 x (4 x (5 x 'a')))))
(x 1 (x 2 (x 3 (x 4 (x 5 'a')))))

