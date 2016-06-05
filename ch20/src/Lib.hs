module Lib where

-- import Data.Foldable
import Data.Monoid

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

---------------------------------------------

data Optional a = Nada | Yep a

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- class Foldable (t :: * -> *) where
--   foldMap :: Monoid m => (a -> m) -> t a -> m
--   fold :: Monoid m => t m -> m

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . Lib.foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . Lib.foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem key values = getAny $ Lib.foldMap (Any . (== key)) values

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' key values = foldr go False values
  where go _ True = True
        go v False = v == key

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr go Nothing
  where go x Nothing = Just x
        go x (Just acc)
          | x `compare` acc == LT = Just x
          | otherwise = Just acc

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr go Nothing
  where go x Nothing = Just x
        go x (Just acc)
          | x `compare` acc == GT = Just x
          | otherwise = Just acc

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ acc -> acc + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = Lib.foldMap id

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs

main :: IO ()
main = do
  putStrLn "Lib.sum"
  putStrLn $ (++) "    " $ show $ Lib.sum ([1..4] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.sum ([] :: [Integer])
  putStrLn "Lib.product"
  putStrLn $ (++) "    " $ show $ Lib.product ([1..4] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.product ([] :: [Integer])
  putStrLn "Lib.elem"
  putStrLn $ (++) "    " $ show $ Lib.elem 3 ([] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.elem 3 ([1..4] :: [Integer])
  putStrLn "Lib.elem'"
  putStrLn $ (++) "    " $ show $ Lib.elem' 3 ([] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.elem' 3 ([1..4] :: [Integer])
  putStrLn "Lib.minimum"
  putStrLn $ (++) "    " $ show $ Lib.minimum ([] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.minimum ([1..4] :: [Integer])
  putStrLn "Lib.maximum"
  putStrLn $ (++) "    " $ show $ Lib.maximum ([] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.maximum ([1..4] :: [Integer])
  putStrLn "Lib.null"
  putStrLn $ (++) "    " $ show $ Lib.null ([] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.null ([1..4] :: [Integer])
  putStrLn "Lib.length"
  putStrLn $ (++) "    " $ show $ Lib.length ([] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.length ([1..4] :: [Integer])
  putStrLn "Lib.toList"
  putStrLn $ (++) "    " $ show $ Lib.toList ([] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.toList ([1..4] :: [Integer])
  putStrLn "Lib.fold"
  putStrLn $ (++) "    " $ show $ Lib.fold ([] :: [Sum Integer])
  putStrLn $ (++) "    " $ show $ Lib.fold ([Sum 1, Sum 2] :: [Sum Integer])
  putStrLn "Lib.foldMap"
  putStrLn $ (++) "    " $ show $ Lib.foldMap Sum ([] :: [Integer])
  putStrLn $ (++) "    " $ show $ Lib.foldMap Sum ([1..4] :: [Integer])
