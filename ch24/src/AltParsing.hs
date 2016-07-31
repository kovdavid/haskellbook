{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
123abc
as43
456
def
|]

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

main :: IO ()
main = do
  print $ parseString parseNos mempty eitherOr
  print $ parseString (many parseNos) mempty eitherOr
  -- print $ parseString (some letter) mempty a
  -- print $ parseString integer mempty b
  -- print $ parseString parseNos mempty a
  -- print $ parseString parseNos mempty b
  -- print $ parseString (many parseNos) mempty c
  -- print $ parseString (some parseNos) mempty c
