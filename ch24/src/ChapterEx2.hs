module ChapterEx2 where

import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead
import Data.Functor
import Data.Monoid
import Debug.Trace

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

charToInt :: Num a => Char -> a
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

base10Integer :: Parser Integer
base10Integer = do
  str <- many parseDigit
  let num = foldr (\c acc -> acc*10 + (charToInt c)) 0 $ reverse str
  return num

base10Integer' :: Parser Integer
base10Integer' = do
  neg <- (char '-') <|> (char '+') <|> (return ' ')
  num <- base10Integer
  case neg of
    '-' -> return $ (-1) * num
    _   -> return num

main :: IO ()
main = do
  print $ "Davs is awesome"
