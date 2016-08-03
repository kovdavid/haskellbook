{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ChapterEx5 where

import Control.Applicative
import Control.Monad
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Parser.LookAhead
import Text.RawString.QQ
import Data.Functor
import Data.Monoid
import Debug.Trace

data Time = Time Int Int deriving (Eq, Show)

log' = [r|
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
endLineOrFile = (void $ newline) <|> eof

skipSpaces :: Parser ()
skipSpaces = void $ many $ (char ' ' <|> tab)

skipComment :: Parser ()
skipComment = do
  try $ skipSpaces >> string "--" >> skipSpaces >> manyTill anyChar endLineOrFile
  return ()

skipEmpty :: Parser ()
skipEmpty = skipSpaces >> endLineOrFile >> return ()

parseDate :: Parser String
parseDate = char '#' >> skipSpaces >> (many $ choice [digit, char '-'])

parseTime :: Parser Time
parseTime = do
  hour <- natural
  char ':'
  min <- natural
  return $ Time (fromIntegral hour) (fromIntegral min)

parseTodo :: Parser String
parseTodo = do
  start <- parseTime
  skipSpaces
  todo <- (manyTill anyChar (skipComment <|> endLineOrFile))
  many $ skipComment <|> skipEmpty <|> eof
  end <- (lookAhead parseTime) <|> (return $ Time 23 59)
  many $ skipComment <|> skipEmpty <|> eof
  return $ (show start) ++ " *" ++ todo ++ "* " ++ (show end)

main :: IO ()
main = do
  print $ parseString skipComment mempty "-- hello\nsad"
  print $ parseString skipEmpty mempty "   \ns"
  print $ parseString parseDate mempty "#  2025-02-07 -- date"
  print $ parseString parseTime mempty "08:00 Something -- hello \n09:00"
  print $ parseString parseTodo mempty "08:00 Something -- hello \n09:00"
  print $ parseString parseTodo mempty "08:00 Something -- hello \n\n\n09:00"
  print $ parseString parseTodo mempty "08:00 Something -- hello \n\n\n#asd"
  print $ parseString (many parseTodo) mempty "08:00 Something hello\n\n\n09:00 SomethingElse\n10:00 asd"
