module Jammin where

import Data.Bool
import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

-- data JamJars =
  -- Jam Fruit Int
  -- deriving (Eq, Show)

data JamJars =
  JamJars { fruit :: Fruit
          , count :: Int }
          deriving (Eq, Show, Ord)

allJam :: [JamJars]
allJam = [ JamJars Peach 3
         , JamJars Apple 5
         , JamJars Blackberry 3
         , JamJars Blackberry 8
         , JamJars Apple 3
         , JamJars Peach 7 ]

jamCount :: [JamJars] -> Int
jamCount jars = sum $ map count jars

mostRow :: JamJars
mostRow = foldr
            (\jam maxJam -> bool maxJam jam ((count jam) > (count maxJam)))
            (head allJam)
            allJam

compareJam :: JamJars -> JamJars -> Ordering
compareJam (JamJars _ x) (JamJars _ y) = compare x y

sortJam :: [JamJars]
sortJam = sortBy compareJam allJam

compareJamType :: JamJars -> JamJars -> Ordering
compareJamType (JamJars x _) (JamJars y _) = compare x y

sortJamType = sortBy compareJamType allJam

groupJam :: [[JamJars]]
groupJam = groupBy (\(JamJars x _) (JamJars y _) -> x == y) sortJamType
