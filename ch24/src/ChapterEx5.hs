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
data Date = Date Int Int Int deriving (Eq, Show)

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

parseDate :: Parser Date
parseDate = do
  char '#'
  many skipSpace
  year <- natural >>= return . fromIntegral
  char '-'
  month <- natural >>= return . fromIntegral
  char '-'
  day <- natural >>= return . fromIntegral
  return $ Date year month day

parseTime :: Parser Time
parseTime = do
  hour <- natural >>= return . fromIntegral
  char ':'
  min <- natural >>= return . fromIntegral
  return $ Time hour min

parseTodo :: Parser String
parseTodo = do
  start <- parseTime
  many skipSpace
  todo <- manyTill anyChar $ (try $ lookAhead $ string "--") <|> (newline >> return "")
  many $ skipComment <|> void newline <|> skipSpace
  end <- (lookAhead parseTime) <|> (return $ Time 23 59)
  many $ skipComment <|> void newline <|> skipSpace
  return $ (show start) ++ " *" ++ todo ++ "* " ++ (show end)

skipComment :: Parser ()
skipComment = do
  optional $ string "--" >> manyTill anyChar (void newline <|> eof)
  return ()

endLineOrFile :: Parser ()
endLineOrFile = (void $ newline) <|> eof

skipSpace :: Parser ()
skipSpace = void $ (char ' ') <|> tab

skipEmpty :: Parser ()
skipEmpty = void $ many $ (void newline) <|> skipSpace

main :: IO ()
main = do
  print $ parseString (many skipSpace) mempty ""
  print $ parseString (many skipSpace) mempty "   "
  print $ parseString (many $ skipSpace) mempty "    \n    \n \n\n  "
  print $ parseString (skipEmpty) mempty "    \n    \n \n\n  "
  print $ parseString skipComment mempty ""
  print $ parseString skipComment mempty "-- hello"
  print $ parseString skipComment mempty "-- hellod"
  print $ parseString parseDate mempty "#  2025-02-07 -- date"
  print $ parseString parseTime mempty "08:00 Something -- hello \n09:00"
  print $ parseString parseTodo mempty "08:00 Something -- hello \n09:00"
  -- print $ parseString parseTodo mempty "08:00 Something -- hello \n\n\n09:00"
  -- print $ parseString parseTodo mempty "08:00 Something -- hello \n\n\n#asd"
  -- print $ parseString (many parseTodo) mempty "08:00 Something hello\n\n\n09:00 SomethingElse\n10:00 asd"
