module Ch11Sumtypes where

import Data.Int

data  BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)
