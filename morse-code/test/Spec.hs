module Main where

import Test.Hspec
import Test.QuickCheck

import Data.List (intersperse)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "Zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "One"

  describe "digits" $ do
    it "[1]" $ do
      digits 1 `shouldBe` [1]
    it "[1, 0, 0]" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "100" $ do
      wordNumber 100 `shouldBe` "One-Zero-Zero"
    it "9001" $ do
      wordNumber 9001 `shouldBe` "Nine-Zero-Zero-One"


digitToWord :: Int -> String
digitToWord 1 = "One"
digitToWord 2 = "Two"
digitToWord 3 = "Three"
digitToWord 4 = "Four"
digitToWord 5 = "Five"
digitToWord 6 = "Six"
digitToWord 7 = "Seven"
digitToWord 8 = "Eight"
digitToWord 9 = "Nine"
digitToWord _ = "Zero"

digits :: Int -> [Int]
digits 0 = []
digits n = (digits $ div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
