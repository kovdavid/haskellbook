module Main where

main :: IO ()
main = return ()

data Silly a b c d = MkSilly a b c d deriving Show

type DocVersion = Int

data EsResultFound a =
  EsResultFound { _version :: DocVersion
                , _source :: a
  } deriving (Eq, Show)

-- instance (FromJSON a) => FromJSON (EsResultFound a) where
  -- parseJSON (Object v) = EsResultFound <$>
                         -- v .: "_version" <*>
                         -- v .: "_source"
  -- parseJSON _          = empty

data List a = Nil | Cons a (List a)
