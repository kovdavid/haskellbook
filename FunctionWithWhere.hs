module FunctionWithWhere where

printInc :: (Num a, Show a) => a -> IO ()
printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 :: (Num a, Show a) => a -> IO ()
printInc2 n = let plusTwo = n + 2
              in print plusTwo

printInc2' :: (Num a, Show a) => a -> IO ()
printInc2' n = (\plusTwo -> print plusTwo) (n + 2)
