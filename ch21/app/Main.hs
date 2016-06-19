module Main where

main :: IO ()
main = return ()

-- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
-- traverse  :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- traverse f = sequenceA . fmap f

data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      b <- makeIoOnlyObj res
      return $ Right b

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = (traverse makeIoOnlyObj
              . traverse decodeFn =<<) . fetchFn
