module SemigroupEx where

import Test.QuickCheck
import Data.Semigroup as S

-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity x') = Identity $ x <> x'

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

-- Four

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> _ = BoolDisj False
  _ <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Or

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst _ <> Fst y = Fst y
  Fst _ <> Snd y = Snd y
  Snd x <> Fst _ = Snd x
  Snd x <> Snd _ = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-- Combine

newtype Combine a b = Combine { unCombine :: (a -> b)}

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine f') = Combine $ (\x -> (f x) <> (f' x))

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  -- arbitrary = do
    -- arb1 <- arbitrary :: Arbitrary a => Gen a
    -- arb2 <- arbitrary :: Arbitrary b => Gen b
    -- return (Combine arb1 arb2)

-- Comp

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  _ <> _ = Comp id

-- Validation

data Validation a b = Fail a | Succ b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Fail x <> Fail x' = Fail $ x <> x'
  Fail x <> _ = Fail x
  _ <> Fail x = Fail x
  Succ _ <> Succ x = Succ x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Fail <$> arbitrary, Succ <$> arbitrary]

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

-- AccumulateRight

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Fail x) <> _ = AccumulateRight (Fail x)
  _ <> AccumulateRight (Fail x) = AccumulateRight (Fail x)
  AccumulateRight (Succ x) <> AccumulateRight (Succ x') = AccumulateRight $ Succ $ x <> x'

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = oneof [ AccumulateRight <$> Fail <$> arbitrary
                    , AccumulateRight <$> Succ <$> arbitrary ]

type AccumulateRightAssoc a b = AccumulateRight a b
                             -> AccumulateRight a b
                             -> AccumulateRight a b
                             -> Bool

-- AccumulateBoth

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Succ x) <> AccumulateBoth (Succ x') = AccumulateBoth $ Succ $ x <> x'
  AccumulateBoth (Fail x) <> AccumulateBoth (Fail x') = AccumulateBoth $ Fail $ x <> x'
  AccumulateBoth (Fail x) <> _ = AccumulateBoth (Fail x)
  _ <> AccumulateBoth (Fail x) = AccumulateBoth (Fail x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = oneof [ AccumulateBoth <$> Fail <$> arbitrary
                    , AccumulateBoth <$> Succ <$> arbitrary ]

type AccumulateBothAssoc a b = AccumulateBoth a b
                            -> AccumulateBoth a b
                            -> AccumulateBoth a b
                            -> Bool


main :: IO ()
main = do
  putStrLn "Starting engines.."
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (semigroupAssoc :: ThreeAssoc String String String)
  quickCheck (semigroupAssoc :: FourAssoc String String String String)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc String Int)
  quickCheck (semigroupAssoc :: ValidationAssoc String String)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc String String)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc String String)
