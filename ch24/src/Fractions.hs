{-# LANGUAGE OverloadedStrings #-}

module Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be 0"
    _ -> return (numerator % denominator)

parseFraction' :: (Monad m, TokenParsing m) => m Rational
parseFraction' = do
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
  print $ parseOnly parseFraction' shouldWork
  print $ parseOnly parseFraction' shouldAlsoWork
  print $ parseOnly parseFraction' alsoBad
  print $ parseOnly parseFraction' badFraction

  print $ parseString parseFraction' mempty shouldWork
  print $ parseString parseFraction' mempty shouldAlsoWork
  print $ parseString parseFraction' mempty alsoBad
  print $ parseString parseFraction' mempty badFraction
