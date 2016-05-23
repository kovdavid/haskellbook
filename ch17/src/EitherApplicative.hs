module EitherApplicative where

-- validToEither :: Validation e a -> Either e a
-- validToEither (Failure err) = Left err
-- validToEither (Success a) = Right a

-- eitherToValid :: Either e a -> Validation e a
-- eitherToValid (Left err) = Failure err
-- eitherToValid (Right a) = Success a

-- eitherToValid . validToEither == id
-- validToEither . eitherToValid == id

data Errors =
    DivideByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data Validation err a =
    Failure err
  | Success a
  deriving (Eq, Show)

instance Functor (Validation a) where
  fmap _ (Failure a) = Failure a
  fmap f (Success b) = Success $ f b

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (<*>) (Second f) (Second x) = Second (f x)
  (<*>) (First err) _ = First err
  (<*>) _ (First err) = First err

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f) (Success x) = Success (f x)
  (<*>) (Failure e1) (Failure e2) = Failure $ mappend e1 e2
  (<*>) _ (Failure err) = Failure err
  (<*>) (Failure err) _ = Failure err

applyIfBothSecond :: (Sum e) (a -> b)
                  -> (Sum e) a
                  -> (Sum e) b
applyIfBothSecond (Second f) (Second x) = Second $ f x
applyIfBothSecond (First err) _ = First err
applyIfBothSecond _ (First err) = First err

applyMappendError :: Monoid e =>
                     (Validation e) (a -> b)
                  -> (Validation e) a
                  -> (Validation e) b
applyMappendError (Success f) (Success x) = Success $ f x
applyMappendError (Failure f1) (Failure f2) = Failure $ mappend f1 f2
applyMappendError (Failure f) _ = Failure f
applyMappendError _ (Failure f) = Failure f

main :: IO ()
main = return ()
