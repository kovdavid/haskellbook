{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ChapterEx5 where

import Control.Applicative
import Control.Monad
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Parser.LookAhead
import Text.RawString.QQ
import Debug.Trace
import Test.Hspec
import Data.ByteString

data Time = Time Int Int deriving (Eq, Show)
data Date = Date Int Int Int deriving (Eq, Show)

newtype StartTime = StartTime Time deriving (Eq, Show)
newtype EndTime = EndTime Time deriving (Eq, Show)
newtype LogText = LogText String deriving (Eq, Show)

data LogEntry = LogEntry LogText StartTime EndTime deriving (Eq, Show)

log' :: String
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
  minute <- natural >>= return . fromIntegral
  return $ Time hour minute

--  08:00 Something -- hello \n09:00"

parseLogEntry :: Parser LogEntry
parseLogEntry = do
  let spaces = many skipSpace
      commentStart = lookAhead $ void $ string "--"
      todoTill = (try $ spaces >> commentStart) <|> void newline <|> eof
  start <- parseTime
  text <- manyTill anyChar todoTill
  many skipComment
  end <- (lookAhead parseTime) <|> (return $ Time 23 59)
  return $ LogEntry (LogText text) (StartTime start) (EndTime end)

skipComment :: Parser ()
skipComment = do
  string "--"
  manyTill anyChar (void newline <|> eof)
  return ()

skipSpace :: Parser ()
skipSpace = void $ (char ' ') <|> tab

skipEmpty :: Parser ()
skipEmpty = void newline <|> skipSpace

newtype MyResult a = MyResult (Result a) deriving (Show)

instance Eq a => Eq (MyResult a) where
  (MyResult (Success a)) == (MyResult (Success b)) = a == b
  _ == _ = False

main :: IO ()
main = hspec $ do
    it "skipSpace" $ do
      let p = many (skipSpace :: Parser ())
          res1 = MyResult $ parseString (p >> position) mempty ""
          res2 = MyResult $ parseString (p >> position) mempty " "
          res3 = MyResult $ parseString (p >> position) mempty "  test"
          res4 = MyResult $ parseString (p >> restOfLine) mempty "  test"
      res1 `shouldBe` (MyResult $ Success $ Columns 0 0 :: MyResult Delta)
      res2 `shouldBe` (MyResult $ Success $ Columns 1 1 :: MyResult Delta)
      res3 `shouldBe` (MyResult $ Success $ Columns 2 2 :: MyResult Delta)
      res4 `shouldBe` (MyResult $ Success $ "test" :: MyResult ByteString)
    it "skipEmpty" $ do
      let p = many (skipEmpty :: Parser ())
          res1 = MyResult $ parseString (p >> position) mempty ""
          res2 = MyResult $ parseString (p >> position) mempty "  "
          res3 = MyResult $ parseString (p >> position) mempty "  \n\n  \n"
          res4 = MyResult $ parseString (p >> position) mempty "  \n\n  \n  test"
          res5 = MyResult $ parseString (p >> restOfLine) mempty "  \n\n  \ntest"
      res1 `shouldBe` (MyResult $ Success $ Columns 0 0 :: MyResult Delta)
      res2 `shouldBe` (MyResult $ Success $ Columns 2 2 :: MyResult Delta)
      res3 `shouldBe` (MyResult $ Success $ Lines 3 0 7 0 :: MyResult Delta)
      res4 `shouldBe` (MyResult $ Success $ Lines 3 2 9 2 :: MyResult Delta)
      res5 `shouldBe` (MyResult $ Success $ "test" :: MyResult ByteString)
    it "skipComment" $ do
      let p = many (skipComment :: Parser ())
          res1 = MyResult $ parseString (p >> position) mempty ""
          res2 = MyResult $ parseString (p >> position) mempty "  "
          res3 = MyResult $ parseString (p >> position) mempty "  --Test"
          res4 = MyResult $ parseString (p >> position) mempty "--Test"
          res5 = MyResult $ parseString (p >> position) mempty "--Test\ndavs"
      res1 `shouldBe` (MyResult $ Success $ Columns 0 0 :: MyResult Delta)
      res2 `shouldBe` (MyResult $ Success $ Columns 0 0 :: MyResult Delta)
      res3 `shouldBe` (MyResult $ Success $ Columns 0 0 :: MyResult Delta)
      res4 `shouldBe` (MyResult $ Success $ Columns 6 6 :: MyResult Delta)
      res5 `shouldBe` (MyResult $ Success $ Lines 1 0 7 0 :: MyResult Delta)
    it "parseTime" $ do
      let p = parseTime
          res1 = MyResult $ parseString (p >> position) mempty "12:34"
          res2 = MyResult $ parseString (p >> restOfLine) mempty "12:34 "
          res3 = MyResult $ parseString (p) mempty "12:34"
      res1 `shouldBe` (MyResult $ Success $ Columns 5 5 :: MyResult Delta)
      res2 `shouldBe` (MyResult $ Success $ "" :: MyResult ByteString)
      res3 `shouldBe` (MyResult $ Success $ Time 12 34 :: MyResult Time)
    it "parseDate" $ do
      let p = parseDate
          res1 = MyResult $ parseString p mempty "#2010-02-07"
          res2 = MyResult $ parseString p mempty "# 2010-02-07"
          res3 = MyResult $ parseString p mempty "# 2010-02-07 -- test"
          res4 = MyResult $ parseString (p >> restOfLine) mempty "# 2010-02-07 -- test"
      res1 `shouldBe` (MyResult $ Success $ Date 2010 02 07)
      res2 `shouldBe` (MyResult $ Success $ Date 2010 02 07)
      res3 `shouldBe` (MyResult $ Success $ Date 2010 02 07)
      res4 `shouldBe` (MyResult $ Success $ "-- test")
    it "skipComment <|> skipEmpty" $ do
      let p = many $ skipComment <|> skipEmpty
          res1 = MyResult $ parseString (p >> position) mempty ""
          res2 = MyResult $ parseString (p >> restOfLine) mempty "--x\n\n    \n-- x  \n\n  Davs"
      res1 `shouldBe` (MyResult $ Success $ Columns 0 0 :: MyResult Delta)
      res2 `shouldBe` (MyResult $ Success $ "Davs" :: MyResult ByteString)
    it "parseLogEntry" $ do
      let p = parseLogEntry
          startTime = StartTime (Time 8 0)
          endTime = EndTime (Time 9 0)
          midnight = EndTime (Time 23 59)
          res1 = MyResult $ parseString (p >> position) mempty "08:00 Test"
          res2 = MyResult $ parseString p mempty "08:00 Test"
          res3 = MyResult $ parseString (p >> position) mempty "08:00 Test --asd"
          res4 = MyResult $ parseString p mempty "08:00 Test --asd\n--asd\n09:00"
          res5 = MyResult $ parseString p mempty "08:00 Test --asd\n--asd\n"
          res6 = MyResult $ parseString (p >> restOfLine) mempty "08:00 Test --asd\n--asd\n"
      res1 `shouldBe` (MyResult $ Success $ Columns 10 10 :: MyResult Delta)
      res2 `shouldBe` (MyResult $ Success $ LogEntry (LogText "Test") startTime midnight)
      res3 `shouldBe` (MyResult $ Success $ Columns 16 16 :: MyResult Delta)
      res4 `shouldBe` (MyResult $ Success $ LogEntry (LogText "Test") startTime endTime)
      res5 `shouldBe` (MyResult $ Success $ LogEntry (LogText "Test") startTime midnight)
      res6 `shouldBe` (MyResult $ Success $ "")
    it "many parseLogEntry" $ do
      let p = many parseLogEntry
          startTime8 = StartTime (Time 8 0)
          startTime9 = StartTime (Time 9 0)
          endTime = EndTime (Time 9 0)
          midnight = EndTime (Time 23 59)
          res1 = MyResult $ parseString p mempty "08:00 Test"
          res2 = MyResult $ parseString p mempty "08:00 Test\n09:00 Davs"
      res1 `shouldBe` (MyResult $ Success $ [LogEntry (LogText "Test") startTime8 midnight])
      res2 `shouldBe` (MyResult $ Success $ [ LogEntry (LogText "Test") startTime8 endTime
                                            , LogEntry (LogText "Davs") startTime9 midnight ])
