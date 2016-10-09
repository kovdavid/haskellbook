{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module MonadTrans where

import Web.Scotty
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

val :: ReaderT Int (ReaderT Char IO) Double
val = ReaderT $ \x ->
        ReaderT $ \y -> do
          return 1.0

val' :: ReaderT Int (ReaderT Char IO) Double
val' = do
  x :: Int <- ask
  y :: Char <- lift $ ask
  return 1.0


main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (lift :: IO a -> ActionM a) $ putStrLn "hello"
    (liftIO :: IO a -> ActionM a) $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
