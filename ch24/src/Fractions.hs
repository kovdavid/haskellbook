{-# LANGUAGE OverloadedStrings #-}

module Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be 0"
    _ -> return (numerator % denominator)

type FractionOrDecimal = Either Rational Integer

parseFractionOrDecimal :: Parser FractionOrDecimal
parseFractionOrDecimal =
    (Left <$> try parseFraction)
  <|>
    (Right <$> decimal)

main :: IO ()
main = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction
