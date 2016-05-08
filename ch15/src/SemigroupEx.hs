module SemigroupEx where

import Test.QuickCheck
import Data.Monoid hiding ((<>))
import Data.Semigroup as S

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity x') = Identity $ x <> x'

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity $ mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

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

instance Monoid BoolConj where
  mappend = (<>)
  mempty = BoolConj True

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> _ = BoolDisj False
  _ <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mappend = (<>)
  mempty = BoolDisj True

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

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mappend = (<>)
  mempty = Combine $ \_ -> mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  -- arbitrary = do
    -- arb1 <- arbitrary :: Arbitrary a => Gen a
    -- arb2 <- arbitrary :: Arbitrary b => Gen b
    -- return (Combine arb1 arb2)

-- Comp

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  _ <> _ = Comp id

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp $ id
  mappend = (<>)

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

-- ######################## --
-- ####### MonoidEx ####### --
-- ######################## --

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)

  (Mem g) `mappend` (Mem g') =
    Mem $ (\s ->
      let (a1, s1) = g s
          (a2, s2) = g' s1
       in (a1 `mappend` a2, s2)
    )

f' :: Num s => Mem s String
f' = Mem $ \s -> ("hi", s + 1)

zero :: Int
zero = 0

memRun :: IO ()
memRun = do
  print $ runMem (f' `mappend` mempty) zero
  print $ runMem (mempty `mappend` f') zero
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' `mappend` mempty) zero == runMem f' zero
  print $ runMem (mempty `mappend` f') zero == runMem f' zero

main :: IO ()
main = do
  putStrLn "Starting engines.."

  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

  quickCheck (semigroupAssoc :: ThreeAssoc String String String)
  quickCheck (semigroupAssoc :: FourAssoc String String String String)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  quickCheck (semigroupAssoc :: OrAssoc String Int)
  quickCheck (semigroupAssoc :: ValidationAssoc String String)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc String String)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc String String)

  putStrLn "Liftoff.."
