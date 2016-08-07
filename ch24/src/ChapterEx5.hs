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

type DateEntry = (Date, [LogEntry])

data Log = Log [(Date, [LogEntry])] deriving (Eq, Show)

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
  let spaces' = many skipSpace
      commentStart = lookAhead $ void $ string "--"
      todoTill = (try $ spaces' >> commentStart) <|> void newline <|> eof
  start <- parseTime
  logText <- manyTill anyChar todoTill
  many skipComment
  end <- (lookAhead parseTime) <|> (return $ Time 23 59)
  return $ LogEntry (LogText logText) (StartTime start) (EndTime end)

parseDateEntry :: Parser DateEntry
parseDateEntry = do
  many $ skipComment <|> skipEmpty
  date <- parseDate
  many $ skipComment <|> skipEmpty
  logEntries <- many parseLogEntry :: Parser [LogEntry]
  return (date, logEntries)

parseLog :: Parser Log
parseLog = do
  l <- many parseDateEntry
  return $ Log l

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
      res1 `shouldBe` (MyResult $ Success $ [ LogEntry (LogText "Test") startTime8 midnight ])
      res2 `shouldBe` (MyResult $ Success $ [ LogEntry (LogText "Test") startTime8 endTime
                                            , LogEntry (LogText "Davs") startTime9 midnight ])
    it "parseDateEntry" $ do
      let p = parseDateEntry
          str = "\n--Test\n# 2010-02-02 --Hello\n08:00 Test --asd\n09:00 Test2\n10:00 Test3"
          res = MyResult $ parseString p mempty str
          date = Date 2010 02 02
          log1 = LogEntry (LogText "Test") (StartTime $ Time 8 0) (EndTime $ Time 9 0)
          log2 = LogEntry (LogText "Test2") (StartTime $ Time 9 0) (EndTime $ Time 10 0)
          log3 = LogEntry (LogText "Test3") (StartTime $ Time 10 0) (EndTime $ Time 23 59)
      res `shouldBe` (MyResult $ Success $ (date, [log1, log2, log3]))

    it "parseLog" $ do
      let res = MyResult $ parseString parseLog mempty log'
      res `shouldBe` MyResult (
        Success (
            Log [ (Date 2025 2 5,
                    [ LogEntry (LogText "Breakfast") (StartTime (Time 8 0)) (EndTime (Time 9 0))
                    , LogEntry (LogText "Sanitizing moisture collector") (StartTime (Time 9 0)) (EndTime (Time 11 0))
                    , LogEntry (LogText "Exercising in high-grav gym") (StartTime (Time 11 0)) (EndTime (Time 12 0))
                    , LogEntry (LogText "Lunch") (StartTime (Time 12 0)) (EndTime (Time 13 0))
                    , LogEntry (LogText "Programming") (StartTime (Time 13 0)) (EndTime (Time 17 0))
                    , LogEntry (LogText "Commuting home in rover") (StartTime (Time 17 0)) (EndTime (Time 17 30))
                    , LogEntry (LogText "R&R") (StartTime (Time 17 30)) (EndTime (Time 19 0))
                    , LogEntry (LogText "Dinner") (StartTime (Time 19 0)) (EndTime (Time 21 0))
                    , LogEntry (LogText "Shower") (StartTime (Time 21 0)) (EndTime (Time 21 15))
                    , LogEntry (LogText "Read") (StartTime (Time 21 15)) (EndTime (Time 22 0))
                    , LogEntry (LogText "Sleep") (StartTime (Time 22 0)) (EndTime (Time 23 59)) ])
                , (Date 2025 2 7,
                    [ LogEntry (LogText "Breakfast") (StartTime (Time 8 0)) (EndTime (Time 9 0))
                    , LogEntry (LogText "Bumped head, passed out") (StartTime (Time 9 0)) (EndTime (Time 13 36))
                    , LogEntry (LogText "Wake up, headache") (StartTime (Time 13 36)) (EndTime (Time 13 37))
                    , LogEntry (LogText "Go to medbay") (StartTime (Time 13 37)) (EndTime (Time 13 40))
                    , LogEntry (LogText "Patch self up") (StartTime (Time 13 40)) (EndTime (Time 13 45))
                    , LogEntry (LogText "Commute home for rest") (StartTime (Time 13 45)) (EndTime (Time 14 15))
                    , LogEntry (LogText "Read") (StartTime (Time 14 15)) (EndTime (Time 21 0))
                    , LogEntry (LogText "Dinner") (StartTime (Time 21 0)) (EndTime (Time 21 15))
                    , LogEntry (LogText "Read") (StartTime (Time 21 15)) (EndTime (Time 22 0))
                    , LogEntry (LogText "Sleep") (StartTime (Time 22 0)) (EndTime (Time 23 59)) ]) ]))
