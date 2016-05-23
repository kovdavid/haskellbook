module ApplicativeLaws where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- pure id <*> v = v

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend x Fools = x
  mappend Fools x = x
  mappend _ _ = Twoo

instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch (applicative [("b", "w", 1 :: Int)])
