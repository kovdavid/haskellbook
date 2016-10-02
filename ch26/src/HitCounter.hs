{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let count = (M.findWithDefault 0 k m) + 1
   in (M.insert k count m, count)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed :: Text <- param "key"
    config <- lift ask
    let key' = mappend (prefix config) unprefixed
    counts' <- liftIO $ readIORef $ counts config
    let (newCounts', newInteger) = bumpBoomp key' counts'
    liftIO $ writeIORef (counts config) newCounts'
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1><br />Key was: "
                   , key'
                   ]

data Hole = Hole

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArg
      runR = \response -> runReaderT response config
  scottyT 3000 runR app

