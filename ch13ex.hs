module Main where

import Control.Monad (forever)
import Data.Char (toLower, isAlphaNum)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
    True -> putStrLn "palindrome"
    False -> do
      putStrLn "Nope"
      exitSuccess

isPalindrome :: String -> Bool
isPalindrome str =
  let filteredStr = (filter isAlphaNum) . (filter (/= ' ')) . (map toLower) $ str
   in filteredStr == reverse filteredStr

main :: IO ()
main = palindrome


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                        "Name was: " ++ show name ++
                        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Gimme a name: "
  name <- getLine

  putStr "Gimme an age: "
  ageStr <- getLine

  let age = read ageStr
   in case mkPerson name age of
        (Right person) -> putStrLn $ "YAY " ++ show person
        (Left err) -> putStrLn $ "Error " ++ show err

  return ()
