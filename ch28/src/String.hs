{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified System.IO as SIO

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UTF8

s :: String
s = "\12371\12435"

utf8ThenPrint :: B.ByteString -> IO ()
utf8ThenPrint = putStrLn . T.unpack . TE.decodeUtf8

throwsException :: IO ()
throwsException = utf8ThenPrint $ B8.pack s

bytesByWayOfText :: B.ByteString
bytesByWayOfText = TE.encodeUtf8 (T.pack s)

libraryDoesTheWork :: B.ByteString
libraryDoesTheWork = UTF8.fromString s

thisWorks :: IO ()
thisWorks = utf8ThenPrint bytesByWayOfText

alsoWorks :: IO ()
alsoWorks = utf8ThenPrint libraryDoesTheWork


dictWords :: IO String
dictWords = SIO.readFile "/usr/share/dict/words"

dictWordsT :: IO T.Text
dictWordsT = TIO.readFile "/usr/share/dict/words"

main :: IO ()
main = do
  replicateM_ 10 (dictWords >>= print)
  replicateM_ 10 (dictWordsT >>= TIO.putStrLn)

