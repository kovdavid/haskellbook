{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module MonadTrans where

import Web.Scotty
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (lift :: IO a -> ActionM a) $ putStrLn "hello"
    (liftIO :: IO a -> ActionM a) $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
