{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data Test1 = Test1
  { _name :: String
  , _age :: Integer
  } deriving (Show, Generic)

data Test2 = Test2
  { _id :: Integer
  , _people :: [Test1]
  } deriving (Show, Generic)

json_data1 = "{\"name\":\"name1\",\"age\":10}"
json_data2 = "{\"id\":4,\"people\":[{\"name\":\"name1\",\"age\":10},{\"name\":\"name2\",\"age\":12}]}"
json_data3 = "{\"new_key\":5,\"id\":4,\"people\":[{\"name\":\"name1\",\"age\":10},{\"name\":\"name2\",\"age\":12}]}"

instance FromJSON Test1 where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
instance FromJSON Test2 where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

main :: IO ()
main = do
  let x :: Maybe Test2 =  decode json_data3
  print $ show $ x
  return ()

