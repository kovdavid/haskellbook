{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty
-- import Web.Scotty.Internal.Types (ActionT(..))
-- import Data.Monoid (mconcat)
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.State.Lazy hiding (get)
import Control.Monad.IO.Class

main :: IO ()
main =
  scotty 3000 $
  do get "/:word" $
       do beam <- param "word"
          liftIO (putStrLn "hello")
          html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-- main :: IO ()
-- main =
    -- scotty 3000 $
    -- do get "/:word" $
           -- do beam <- param "word"
              -- -- (lift :: IO a -> ActionM a) $ putStrLn "hello"
              -- ActionT .
                  -- (ExceptT . fmap Right) .
                  -- (ReaderT . const) .
                  -- (\m ->
                        -- StateT
                            -- (\s -> do
                                 -- a <- m
                                 -- return (a, s))) $
                  -- putStrLn "hello"
              -- html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-- instance MonadTrans (ActionT e) where
  -- lift = ActionT . lift . lift . lift

-- newtype ActionT e m a = ActionT {
  -- runAM :: Except (ActionError e)
                  -- (ReaderT ActionEnv
                           -- (StateT ScottyRespose m)) a
-- } deriving (Functor, Applicative)

-- instance MonadTrans (ExceptT e) where
  -- lift = ExceptT . liftM Right
