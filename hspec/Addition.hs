module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

main :: IO ()
main = hspec $ do
  describe "QuickCheck" $ do
    it "works" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Addition" $ do
    it "15 dividedBy 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 dividedBy 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
  describe "Myltiplu" $ do
    it "multiplies" $ do
      mul' 3 5 `shouldBe` 15

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)


mul' :: (Integral a) => a -> a -> a
mul' x y = go x y 0
  where go x' y' acc
         | x' == 0 = acc
         | otherwise = go (x' - 1) y' (acc + y')

trivialInt :: Gen Int
trivialInt = return 1

oneToThree :: Gen Int
oneToThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genTreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

values :: IO ()
values = sample (arbitrary :: Gen Int)

-- QC withoug HSpec

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
