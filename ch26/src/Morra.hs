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
import System.Exit
import System.Random
import qualified Text.Read as TR

data Choice = Odds | Evens | Exit deriving (Eq, Ord, Show)
data Player = Player1 | Player2 | CPU deriving (Eq, Show)

data Score = Score
  { playerScore :: Integer
  , computerScore :: Integer
  } deriving Show

data GameState = GameState
  { gameScore :: Score
  , gameSeed :: StdGen
  } deriving Show

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

getComputerChoice :: Choice -> Choice
getComputerChoice Odds = Evens
getComputerChoice Evens = Odds

main :: IO ()
main = do
  let gameState = GameState (Score 0 0) (mkStdGen 0)
  (_, finalGameState) <- runStateT mainLoop gameState
  print $ show $ finalGameState
  putStrLn "Bye"


mainLoop :: StateT GameState IO ()
mainLoop = do
    gameState <- get
    playerChoice <- lift $ askPlayerChoice

    when (playerChoice /= Exit) $ do
        let computerChoice = getComputerChoice playerChoice
        playerNumber <- lift $ askPlayerNumber
        let (computerNumber, newGameSeed) = randomR (0, playerNumber+1) $ gameSeed gameState

        liftIO $ print $ "PlayerChoice: " ++ show playerChoice
        liftIO $ print $ "PlayerNumber: " ++ show playerNumber
        liftIO $ print $ "ComputerChoice: " ++ show computerChoice
        liftIO $ print $ "ComputerNumber: " ++ show computerNumber

        let currentGameScore = gameScore gameState
        let newGameScore = Score ((playerScore currentGameScore) + 1) (computerScore currentGameScore)

        put $ GameState newGameScore newGameSeed

        mainLoop
