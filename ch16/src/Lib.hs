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
--
--
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
--
-- <$> :: Functor f => (a -> b) -> (f a -> f b)
--
-- fmap1 :: Functor f1 => (b -> c) -> (f1 b -> f1 c)
-- fmap2 :: Functor f2 => (a -> b) -> (f2 a -> f2 b)
--
--  b = b -> c
--
-- (.) :: (y -> z) -> (x -> y) -> (x -> z)
-- fmap1 :: Functor f1 => (b -> c) -> (f1 b -> f1 c)
-- fmap2 :: Functor f2 => (e -> h) -> (f2 e -> f2 h)
--
--
-- fmap1 :: (Maybe Int -> Maybe Char) -> ([] Maybe Int -> [] Maybe Char)
-- fmap2 :: (Int -> Char) -> (Maybe Int -> Maybe Char)
--
-- (.) fmap1 :: (x -> (Maybe Int -> Maybe Char)) -> (x -> ([] Maybe Int -> [] Maybe Char))
--     fmap2 :: (Int -> Char) -> ([] Maybe Int -> [] Maybe Char)
--
--
--
-- (fmap1 . fmap2)
-- (.) fmap1    (y -> z) == (b -> c) -> (f1 b -> f1 c)
--              y = b -> c
--              z = f1 b -> f1 c
--
--            :: (x -> (b -> c)) -> (x -> (f1 b -> f1 c))
--
-- (.) fmap1 fmap2 (x -> (b -> c)) == (e -> h) -> (f2 e -> f2 h)
--                  x = e -> h
--                  b -> c = f2 e -> f2 h
--                  b = f2 e
--                  c = f2 h
--
--                  (e -> h) -> (f1 f2 e -> f1 f2 h)
