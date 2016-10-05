{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Morra where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import System.Exit
import System.Random
import Text.Read

data Choice = Odds | Evens | Exit deriving (Eq, Ord, Show)

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
  let mInt = readMaybe input
  case mInt of
    Just int -> return int
    Nothing -> putStr "Wrong input. " >> askPlayerNumber

getComputerChoice :: Choice -> Choice
getComputerChoice Odds = Evens
getComputerChoice Evens = Odds

main :: IO ()
main = do
  let gameState = GameState (Score 0 0) (mkStdGen 0)
  mainLoop
  putStrLn "Bye"


mainLoop :: IO ()
mainLoop = do
    playerChoice <- askPlayerChoice

    when (playerChoice /= Exit) $ do
        let computerChoice = getComputerChoice playerChoice
        playerNumber <- askPlayerNumber
        -- computerNumber <- getComputerNumber
        print $ "PlayerChoice: " ++ show playerChoice
        print $ "PlayerNumber: " ++ show playerNumber
        print $ "ComputerChoice: " ++ show computerChoice

        mainLoop
