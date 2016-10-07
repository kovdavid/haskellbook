{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Morra where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import System.Random
import qualified Text.Read as TR
import qualified Data.Map.Strict as M

data Choice
  = Odds
  | Evens
  | Exit
  deriving (Eq, Ord, Show)

data Player
  = Player
  | CPU
  deriving (Eq, Show)

data Score = Score
  { _playerScore :: Integer
  , _computerScore :: Integer
  } deriving (Show)

type LastPlayerNumbers = (Maybe Integer, Maybe Integer)

data GameState = GameState
  { _gameScore :: Score
  , _gameRound :: Integer
  , _player3Grams :: M.Map LastPlayerNumbers Integer
  , _lastPlayerNumbers :: LastPlayerNumbers
  } deriving (Show)

initialGameState :: GameState
initialGameState =
  GameState
  { _gameScore = Score 0 0
  , _gameRound = 0
  , _player3Grams = M.empty
  , _lastPlayerNumbers = (Nothing, Nothing)
  }

validateChoice :: Monad m => String -> m (Maybe Choice)
validateChoice "o" = return $ Just Odds
validateChoice "e" = return $ Just Evens
validateChoice "x" = return $ Just Exit
validateChoice _ = return $ Nothing

askPlayerChoice :: IO Choice
askPlayerChoice = do
  putStr "[o]dds or [e]vens or e[x]it? "
  mChoice <- getLine >>= validateChoice
  case mChoice of
    Just choice -> return choice
    Nothing -> putStr "Invalid input. " >> askPlayerChoice

askPlayerNumber :: IO Integer
askPlayerNumber = do
  putStr "Your number: "
  input <- getLine
  let mInt = TR.readMaybe input
  case mInt of
    Just num ->
      if num >= 0
        then return num
        else putStr "Input must be positive. " >> askPlayerNumber
    Nothing -> putStr "Wrong input. " >> askPlayerNumber

main :: IO ()
main = do
  (_, finalGameState) <- runStateT mainLoop initialGameState
  print $ show $ finalGameState
  putStrLn "Bye"

getWinner :: Integer -> Choice -> Player
getWinner sum player1Choice = go (odd sum) player1Choice
  where
    go True Odds = Player
    go False Evens = Player
    go _ _ = CPU

updateGameScore :: Score -> Player -> Score
updateGameScore (Score p c) Player = Score (p + 1) c
updateGameScore (Score p c) CPU = Score p (c + 1)

increaseGameRound :: GameState -> GameState
increaseGameRound state = state { _gameRound = (_gameRound state) + 1 }

savePlayerNumber :: Integer -> GameState -> GameState
savePlayerNumber num_new state =
  let (_, num_old) = _lastPlayerNumbers state
      playerNumbers = (num_old, Just num_new)
      playerGrams = M.insert (_lastPlayerNumbers state) num_new (_player3Grams state)
   in state { _lastPlayerNumbers = playerNumbers, _player3Grams = playerGrams }

getComputerNumber :: StateT GameState IO Integer
getComputerNumber = do
  state <- get
  let guess = M.lookup (_lastPlayerNumbers state) (_player3Grams state)
  case guess of
    Nothing -> lift $ randomRIO (0, 10) -- random
    Just x -> return x -- guess based on past choices

mainLoop :: StateT GameState IO ()
mainLoop = do
    gameState <- get
    playerChoice <- lift $ askPlayerChoice

    when (playerChoice /= Exit) $ do
        modify increaseGameRound

        computerNumber <- getComputerNumber
        playerNumber <- lift $ askPlayerNumber

        let winner = getWinner (playerNumber + computerNumber) playerChoice

        liftIO $ print $ "PlayerChoice: " ++ show playerChoice
        liftIO $ print $ "PlayerNumber: " ++ show playerNumber
        liftIO $ print $ "ComputerNumber: " ++ show computerNumber
        liftIO $ print $ "Winner: " ++ show winner

        modify $ (\s -> s { _gameScore = updateGameScore (_gameScore gameState) winner })
        modify $ savePlayerNumber playerNumber

        mainLoop
