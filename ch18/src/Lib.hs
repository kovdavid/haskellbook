module Lib where

import Control.Monad (join, (>=>))
import Control.Applicative ((*>))

-- let andOne x = [x , 1]
--
-- fmap andOne [4, 5, 6]
-- concat $ fmap andOne [4, 5, 6]

-- _ = join :: Monad m => m (m a) -> m a

-- bind :: Monad m => (a -> m b) -> m a -> m b
-- bind f v = join $ fmap f v

sequencing :: IO ()
sequencing = do
  putStrLn "foo"
  putStrLn "bar"

sequencing' :: IO ()
sequencing' = do
  putStrLn "foo" >> putStrLn "bar"

sequencing'' :: IO ()
sequencing'' = do
  putStrLn "foo" *> putStrLn "bar"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = do
  getLine >>= putStrLn

bindAndSequence :: IO ()
bindAndSequence = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("hello: " ++ name)

bindAndSequence' :: IO ()
bindAndSequence' = do
  putStrLn "name pls:"
  >> getLine
  >>= \name -> putStrLn ("hello: " ++ name)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "How old are you?"
