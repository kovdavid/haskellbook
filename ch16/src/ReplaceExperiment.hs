module ReplaceExperiment where

import Test.QuickCheck
import Test.QuickCheck.Function

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

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
twiceLifted' = (fmap . fmap) replaceWithP

thriceLifted :: (Functor f2, Functor f1, Functor f) =>
                f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read <$> ("123" ++) <$> show <$> ioi
     in (*3) <$>  changed

data Two a b = Two a b deriving (Eq, Show)
data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) =>
                   f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- Pair

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

-- Two

-- declared before
-- data Two a b = Two a b

-- declared before
-- instance Functor (Two a) where
  -- fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

-- Three'

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

-- Four

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing  = Nothing

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left l)  = Left l

data Sum' a b = First' a | Second' b deriving (Eq, Show)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers x) = Yeppers $ f x
  fmap _ LolNope = LolNope

instance Functor (Sum' a) where
  fmap f (Second' b) = Second' $ f b
  fmap _ (First' a) = First' a

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant v) = Constant v

data Wrap f a = Wrap (f a) deriving (Eq, Show)

main :: IO ()
main = do
  putStrLn "Starting engines"

  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: Pair Int -> IntToInt -> IntToInt -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Two Int Int)
  quickCheck (functorCompose' :: Two Int Int -> IntToInt -> IntToInt -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Three Int Int Int)
  quickCheck (functorCompose' :: Three Int Int Int -> IntToInt -> IntToInt -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Three' Int Int)
  quickCheck (functorCompose' :: Three' Int Int -> IntToInt -> IntToInt -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Four' Int Int)
  quickCheck (functorCompose' :: Four' Int Int -> IntToInt -> IntToInt -> Bool)

  quickCheck (functorCompose' :: [] Int -> IntToInt -> IntToInt -> Bool)

-- main :: IO ()
-- main = do
  -- putStr "replaceWithP' lms:"
  -- print (replaceWithP' lms)

  -- putStr "liftedReplace lms:"
  -- print (liftedReplace lms)

  -- putStr "liftedReplace' lms:"
  -- print (liftedReplace' lms)

  -- putStr "twiceLifted lms:"
  -- print (twiceLifted lms)

  -- putStr "twiceLifted' lms:"
  -- print (twiceLifted' lms)

  -- putStr "thriceLifted lms:"
  -- print (thriceLifted lms)

  -- putStr "thriceLifted' lms:"
  -- print (thriceLifted' lms)
