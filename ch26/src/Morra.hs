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
import qualified Data.Map as M

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
  { playerScore :: Integer
  , computerScore :: Integer
  } deriving (Show)

data GameState = GameState
  { gameScore :: Score
  , gameRound :: Integer
  , player3grams :: M.Map (Integer, Integer) Integer
  , lastPlayerNumbers :: (Maybe Integer, Maybe Integer)
  } deriving (Show)

initialGameState :: GameState
initialGameState =
  GameState
  { gameScore = Score 0 0
  , gameRound = 0
  , player3grams = M.empty
  , lastPlayerNumbers = (Nothing, Nothing)
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
increaseGameRound state = state { gameRound = (gameRound state) + 1 }

savePlayerNumber :: Integer -> GameState -> GameState
savePlayerNumber num_new state =
  let (_, num_old) = lastPlayerNumbers state
   in state { lastPlayerNumbers = (num_old, Just num_new)}

mainLoop :: StateT GameState IO ()
mainLoop = do
    gameState <- get
    playerChoice <- lift $ askPlayerChoice

    when (playerChoice /= Exit) $ do
        modify increaseGameRound

        playerNumber <- lift $ askPlayerNumber

        computerNumber <- lift $ randomRIO (0, playerNumber+1)

        let winner = getWinner (playerNumber + computerNumber) playerChoice
        let newGameScore = updateGameScore (gameScore gameState) winner

        modify $ savePlayerNumber playerNumber

        -- put $ GameState newGameScore 0 M.empty (Nothing, Nothing)

        mainLoop

        -- liftIO $ print $ "PlayerChoice: " ++ show playerChoice
        -- liftIO $ print $ "PlayerNumber: " ++ show playerNumber
        -- liftIO $ print $ "ComputerNumber: " ++ show computerNumber
        -- liftIO $ print $ "Winner: " ++ show winner
