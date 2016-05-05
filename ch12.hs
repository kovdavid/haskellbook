module Main where

main :: IO ()
main = return ()

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

replaceThe :: String -> String
replaceThe str = unwords $ map f $ words str
  where f "the" = "a"
        f x = x

isVowel :: Char -> Bool
isVowel c = elem c "aeiouAEIOU"

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (x:xs)
  | isVowel x = True
  | otherwise = False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str =
  let words = wordsAfterThe str
   in fromIntegral $ length $ filter startsWithVowel words

wordsAfterThe :: String -> [String]
wordsAfterThe str = go $ words str
  where go [] = []
        go [x] = []
        go ("the":x:xs) = x : go xs
        go (x:xs) = go xs

countVowels :: String -> Integer
countVowels str = fromIntegral $ length $ filter startsWithVowel $ words str

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str =
  let vowelCount = length $ filter isVowel str
      otherCount = (length str) - vowelCount
   in if vowelCount > otherCount
         then Nothing
         else Just (Word' str)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ go x
    where go 0 = Zero
          go n = Succ (go (n - 1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing = x
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybes
  | hasNothing maybes = Nothing
  | otherwise = Just $ map (\(Just x) -> x) maybes

hasNothing xs = foldr ((||) . isNothing) False xs

lefts' :: [Either a b] -> [a]
lefts' = foldr appendLeft []
  where appendLeft (Right _) acc = acc
        appendLeft (Left x)  acc = x: acc

rights' :: [Either a b] -> [b]
rights' = foldr appendRight []
  where appendRight (Left _)  acc = acc
        appendRight (Right x) acc = x: acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr partition' ([], [])
  where partition' (Left x)  (lefts, rights) = (x : lefts, rights)
        partition' (Right x) (lefts, rights) = (lefts, x : rights)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left x) = f x
either' f g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f m = either' (\_ -> Nothing) (\x -> Just $ f x) m

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Just (x1, x2) -> x1 : myUnfoldr f x2
    Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (y, f y)) x

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Just (x1, x2, x3) -> Node (unfold f x1) x2 (unfold f x3)
    Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f x
         | x == n = Nothing
         | otherwise = Just (x+1, x, x+1)
