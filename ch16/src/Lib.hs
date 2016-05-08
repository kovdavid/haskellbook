module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe String]
lms = [Just "ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) =>
               f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

--          fmap        fmap
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- fmap1 :: Functor f => (m -> n) -> f m -> f n
-- fmap2 :: Functor g => (x -> y) -> g x -> g y

-- (fmap1 . fmap2)

-- (m -> y) -> f (g m) -> f (g y)
