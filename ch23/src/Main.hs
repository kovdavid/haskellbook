{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

-- mkStdGen :: Int -> StdGen
-- next :: g -> (Int, g)
-- random :: (RandomGen g, Random a) => g -> (a, g)
-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)

type Iso a b = (a -> b, b -> a)

newtype Sum a = Sum { getSum :: a }

sumIsIsomorphic :: Iso a (Sum a)
sumIsIsomorphic = (Sum, getSum)

-- newtype State s a = State { runState :: s -> (a, s) }
-- State :: (s -> (a, s)) -> State s a
-- runState :: State s a -> (s -> (a, s))
--
-- random :: (Random a) => StdGen -> (a, StdGen)

data Die =
    DieOne
  | DieTwo
  | DieThree
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree

    x -> error $ "intToDie: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 3) s
      (d2, s2) = randomR (1, 3) s1
      (d3, _) = randomR (1, 3) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 3)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 3))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
           | sum >= 20 = count
           | otherwise =
             let (die, nextGen) = randomR (1, 3) gen
             in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN l g = go l 0 0 g
  where go :: Int -> Int -> Int -> StdGen -> Int
        go limit sum count gen
           | sum >= limit = count
           | otherwise =
             let (die, nextGen) = randomR (1, 3) gen
             in go limit (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged l g = go l 0 (0, []) g
  where go :: Int -> Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go limit sum (count, acc) gen
           | sum >= limit = (count, acc)
           | otherwise =
             let (die, nextGen) = randomR (1, 3) gen
             in go limit (sum + die) (count + 1, acc ++ [intToDie die]) nextGen

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let (x, s2) = g s
    in (f x, s2)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  -- (Moi f) <*> (Moi g) = Moi $ \s ->
    -- let (fun, s2) = f s
        -- (val, s3) = g s2
     -- in (fun val, s3)

  mf <*> mg = Moi $ \s ->
    let (fun, s2) = runMoi mf s
        (val, s3) = runMoi mg s2
     in (fun val, s3)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  mf >>= g = Moi $ \s ->
    let (x, s') = runMoi mf s
     in runMoi (g x) s'

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main = do
  mapM_ (putStrLn . fizzBuzz) [1..20]
