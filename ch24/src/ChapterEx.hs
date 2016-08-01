module ChapterEx where

import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead
import Data.Functor
import Data.Monoid

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer major1 minor1 patch1 _ _) (SemVer major2 minor2 patch2 _ _) =
      (compare major1 major2) <> (compare minor1 minor2) <> (compare patch1 patch2)

-- 1.2.3-4.5.6+7.8.9 => Major 1 $ Minor 2 $ Patch 3 $ Release [4, 5, 6] $ Metadata [7, 8, 9]

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer

  let lookPlusEOF = (void $ lookAhead $ char '+') <|> eof
      nosUntil = (void $ char '.') <|> lookPlusEOF
      releaseP = char '-' >> (manyTill (parseNOSUntil nosUntil) lookPlusEOF)
      metaP = char '+' >> (many $ parseNOSUntil $ charOrEOF '.')

  release' <- try releaseP <|> return []
  meta     <- try metaP    <|> return []

  return $ SemVer major minor patch release' meta

charOrEOF :: Char -> Parser ()
charOrEOF c = void (char c) <|> eof

parseNOSUntil :: Parser a -> Parser NumberOrString
parseNOSUntil stop =
    try $ NOSI <$> integer <* stop
  <|>
    NOSS <$> some alphaNum <* stop

main :: IO ()
main = do
  print $ parseString parseSemVer mempty "10.20.30"
  print $ parseString parseSemVer mempty "10.20.30-2.rel2.3"
  print $ parseString parseSemVer mempty "10.20.30-2.rel2.3+4"
  print $ parseString parseSemVer mempty "10.20.30-2.rel2.3+4.as.1release2"
  -- print $ parseString parseSemVer mempty "1.0.0"
  -- print $ parseString parseSemVer mempty "10.0.0"
  -- print $ parseString parseSemVer mempty "1.2.0"
  -- print $ parseString parseSemVer mempty "10.20.0"
  -- print $ parseString parseSemVer mempty "1.2.3"
  -- print $ parseString parseSemVer mempty "10.20.30"
  -- print $ parseString parseSemVer mempty "10.20.30-release1.davs"
  -- print $ parseString parseNumberOrString mempty "d75s"
  -- print $ parseString parseRelease mempty "d75s.234.asd.123asd"

