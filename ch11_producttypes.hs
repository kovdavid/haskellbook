module Ch11ProductTypes where

data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth deriving (Eq, Show)

data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show)

data JamJars =
  Jam Fruit Int
  deriving (Eq, Show)
