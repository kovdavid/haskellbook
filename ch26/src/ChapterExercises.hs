{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module ChapterExercises where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Functor.Identity

rDec :: Num a => ReaderT a Identity a
rDec = ReaderT $ Identity . flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> print ("Hi: " ++ (show r)) >> return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> print ("Hi: " ++ show s) >> return (show s, s + 1)


-- ========== Fix the code ========== --

isValid :: String -> Bool
isValid v = elem '!' v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

main :: IO ()
main = do
  return ()
