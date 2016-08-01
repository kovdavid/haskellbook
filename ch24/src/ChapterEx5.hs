{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ChapterEx5 where

import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead
import Text.RawString.QQ
import Data.Functor
import Data.Monoid
import Debug.Trace

log = [r|
-- comment

# 2025-02-05

08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

endLineOrFile :: Parser ()
endLineOrFile = (void $ char '\n') <|> eof

skipComment :: Parser ()
skipComment = do
  (optional ((string "--") >> (many $ noneOf "\n") <* endLineOrFile))
  return ()

skipEmpty :: Parser ()
skipEmpty = do
    try $ (void $ whiteSpace) <* endLineOrFile
  <|>
    return ()

parseDate :: Parser String
parseDate = do
  char '#' >> whiteSpace >> (many $ choice [digit, char '-']) <* skipComment

parseTime :: Parser String
parseTime = do
  hour <- count 2 digit
  char ':'
  min <- count 2 digit
  return $ hour ++ ":" ++ min

parseTodo :: Parser String
parseTodo = do
  start <- parseTime
  whiteSpace
  todo <- whiteSpace >> (manyTill letter (void (string "--") <|> endLineOrFile))
  return $ start ++ " DAVS"

main :: IO ()
main = do
  print $ parseString skipComment mempty "-- hello\nsad"
  print $ parseString skipEmpty mempty "   \n"
  print $ parseString parseDate mempty "#  2025-02-07 -- date"
  print $ parseString parseTime mempty "08:00 Something -- hello \n09:00"
  print $ parseString parseTodo mempty "08:00 Something -- hello \n09:00"
  print $ "I got this!"
