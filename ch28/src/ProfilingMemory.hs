module Main where

-- ghc -prof -fprof-auto -rtsopts -O2 main.hs
-- ./main +RTS -hc -p

import Control.Monad

blah :: [Integer]
blah = [1..1000]

main :: IO ()
main =
  replicateM_ 10000 (print blah)
