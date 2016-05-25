module Lib
    ( someFunc
    , bind
    ) where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f v = join $ fmap f v

someFunc :: IO ()
someFunc = putStrLn "someFunc"
