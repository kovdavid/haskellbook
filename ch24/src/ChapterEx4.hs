module ChapterEx2 where

import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead
import Data.Functor
import Data.Monoid
import Debug.Trace

type NumberintPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberintPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  optional (string "1-")
  optional (char '(')
  area <- count 3 digit
  optional (char ')')
  optional (oneOf " -")
  exchange <- count 3 digit
  optional (oneOf " -")
  line <- count 4 digit

  return $ PhoneNumber (read area) (read exchange) (read line)

main :: IO ()
main = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
