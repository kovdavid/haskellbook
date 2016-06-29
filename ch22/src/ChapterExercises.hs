module ChapterExercises where

import Prelude hiding (lookup, uncurry)
import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup key ((a, b):xs)
  | key == a = Just b
  | otherwise = lookup key xs

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x y

x1 :: Maybe (Integer, Integer)
x1 = pure (,) <*> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = pure (,) <*> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
-- x3 n = (,) (z' n) (z' n)
x3 = pure (,) <*> z' <*> z'

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = fmap summed $ (,) <$> xs <*> ys

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed $ (,) <$> xs <*> zs
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7

  print $ foldr (&&) True $ sequA 6
  print $ sequA $ fromMaybe 0 $ s'
  print $ bolt $ fromMaybe 0 $ ys
  print $ bolt $ fromMaybe 0 $ z' 4
