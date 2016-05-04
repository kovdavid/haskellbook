module Ch11 where

-- newtype can have only a single unary constructor
-- newtype has no runtime overhead; it reuses the representation of the type it contains
-- the difference between newtype and its type is gone after compilation
-- with newtype, we can define different typeclasses, as the original type

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows  = Cows Int  deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- tooManyGoats :: Int -> Bool
-- tooManyGoats n = n > 5

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 5
