{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' <* eof

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' <* eof

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

str1 :: Parser String
str1 = string "123" <* eof

-- type Parser a = String -> Maybe (a, String)

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParse' :: Parser String -> IO ()
testParse' p =
  print $ parseString p mempty "123"

main :: IO ()
main = do
  putStrLn "hello world"
