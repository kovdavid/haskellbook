module ChapterExercises where

import Data.Monoid

data Constant a b = Constant a deriving (Show)

instance Foldable (Constant a) where
  foldMap _ _ = mempty
  foldr _ acc _ = acc

-----------------------------------------------------

data Two a b = Two a b deriving (Show)

instance Foldable (Two a) where
  foldMap f (Two _ x) = f x
  foldr f acc (Two _ x) = f x acc

-----------------------------------------------------

data Three a b c = Three a b c deriving (Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x
  foldr f acc (Three _ _ x) = f x acc

-----------------------------------------------------

data Three' a b = Three' a b b deriving (Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = f x <> f y
  foldr f acc (Three' _ x y) = f x $ f y acc

-----------------------------------------------------

data Four' a b = Four' a b b b deriving (Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ x y z) = f x <> f y <> f z
  foldr f acc (Four' _ x y z) = f x $ f y $ f z acc

-----------------------------------------------------

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f xs = foldMap go xs
  where go x
          | f x == True = pure x
          | otherwise = mempty

b :: Maybe a -> a
b x = undefined

c :: Either String Int
c = Right "hello"

main :: IO ()
main = do
  putStrLn "====== Constant a b"
  print $ foldMap id (Constant 5 :: Constant Int [Int])
  print $ foldMap (:[]) (Two 5 7 :: Two Int Int)
  print $ foldMap (Sum) (Three 0 5 7 :: Three Int Int Int)
  print $ foldMap (Sum) (Three' 0 5 7 :: Three' Int Int)
  print $ foldMap (Product) (Four' 9 3 5 7 :: Four' Int Int)
  print $ foldr (+) 0 (Constant 5 :: Constant Int Int)
  print $ foldr (:) [] (Two 5 7 :: Two Int Int)
  print $ foldr (+) 0 (Three 0 5 7 :: Three Int Int Int)
  print $ foldr (+) 0 (Three' 0 5 7 :: Three' Int Int)
  print $ foldr (*) 1 (Four' 9 3 5 7 :: Four' Int Int)
  print $ (filterF even [1..5] :: [Int])
