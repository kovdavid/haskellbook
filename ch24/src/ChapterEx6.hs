module ChapterEx6 where

import Text.Trifecta
import Data.Word
import Data.Bits

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

validateOctet :: Monad m => Integer -> m Integer
validateOctet octet =
  if octet >= 0 && octet <= 255
    then return octet
    else fail $ "Not a valid IPv4 octet:" ++ (show octet)

parseIPv4Octets :: Parser (Integer, Integer, Integer, Integer)
parseIPv4Octets = do
  o1 <- integer >>= validateOctet
  char '.'
  o2 <- integer >>= validateOctet
  char '.'
  o3 <- integer >>= validateOctet
  char '.'
  o4 <- integer >>= validateOctet
  return (o1, o2, o3, o4)

parseIPv4 :: Parser IPAddress
parseIPv4 = do
  (o1, o2, o3, o4) <- parseIPv4Octets
  let x1 = shift o1 24
      x2 = shift o2 16
      x3 = shift o3 8
  return $ IPAddress $ fromIntegral $ x1 + x2 + x3 + o4

main :: IO ()
main = do
  print $ parseString parseIPv4Octets mempty "123.123.1.231"
  print $ parseString parseIPv4Octets mempty "123.123.0.231"
  print $ parseString parseIPv4 mempty "123.123.0.231"
  print $ parseString parseIPv4 mempty "172.16.254.1"
  print $ parseString parseIPv4 mempty "204.120.0.15"
  return ()
