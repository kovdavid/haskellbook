module Main where

import Test.Hspec
import Hangman

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "good guess" $ do
      let guessChar = 'd'
          p  = Puzzle "davs" [Nothing, Nothing, Nothing, Nothing] []
          p' = Puzzle "davs" [Just guessChar, Nothing, Nothing, Nothing] [guessChar]
       in fillInCharacter p guessChar `shouldBe` p'
    it "bad guess" $ do
      let guessChar = 'x'
          p  = Puzzle "davs" [Nothing, Nothing, Nothing, Nothing] []
          p' = Puzzle "davs" [Nothing, Nothing, Nothing, Nothing] [guessChar]
       in fillInCharacter p guessChar `shouldBe` p'

  describe "handleGuess" $ do
    it "good guess" $ do
      let guessChar = 'd'
          p  = Puzzle "davs" [Nothing, Nothing, Nothing, Nothing] []
          p' = Puzzle "davs" [Just guessChar, Nothing, Nothing, Nothing] [guessChar]
       in do
         resultPuzzle <- handleGuess p guessChar
         resultPuzzle `shouldBe` p'
    it "bad guess" $ do
      let guessChar = 'x'
          p  = Puzzle "davs" [Nothing, Nothing, Nothing, Nothing] []
          p' = Puzzle "davs" [Nothing, Nothing, Nothing, Nothing] [guessChar]
       in do
         resultPuzzle <- handleGuess p guessChar
         resultPuzzle `shouldBe` p'
