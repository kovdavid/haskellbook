module ChapterEx7 where

import Text.Trifecta
import Data.Word
import Data.Bits
import Data.Char
import Debug.Trace
import Data.List
import Control.Monad

data IPAddress6 = IPAddress Word64 Word64 deriving (Eq, Ord, Show)

hexCharToInt :: Integral a => Char -> a
hexCharToInt c =
  case c of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'a' -> 10
    'b' -> 11
    'c' -> 12
    'd' -> 13
    'e' -> 14
    'f' -> 15

hexStringToInt :: (Bits a, Integral a) => [Char] -> a
hexStringToInt str = go 0 str
  where go acc [] = acc
        go acc (x:xs) =
          let acc' = (shift acc 4) + (hexCharToInt $ toLower x)
          in go acc' xs

parseHexPart :: Parser [Char]
parseHexPart = do
  h <- many $ oneOf "0123456789abcdefABCDEF"
  case length h > 4 of
    False -> return h
    True -> fail $ "IPv6 hex part too big: " ++ h

parseHex :: Parser Integer
parseHex = do
  hex <- parseHexPart
  return $ hexStringToInt $ hex

main :: IO ()
main = do
  print $ parseString parseHexPart mempty "DEADDDDD"
  print $ parseString parseHex mempty "DEAD"
  -- print $ parseString parseIPv4 mempty "204.120.0.15"
  return ()
