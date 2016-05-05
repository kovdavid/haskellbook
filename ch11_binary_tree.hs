module Main where

main :: IO ()
main = return ()

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = (inorder left) ++ [x] ++ (inorder right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc t = helper t acc
  where helper Leaf = id
        helper (Node l z r) = helper r . f z . helper l

foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' _ acc Leaf = acc
foldTree' f acc (Node left x right) =
  let accLeft = foldTree' f acc left
      accNode = f x accLeft
  in  foldTree' f accNode right

-- foldTree f acc (Node left x right) = undefined
-- foldTree f acc (Node Leaf x right) = foldTree f (f x acc) right
-- foldTree f acc (Node left x Leaf)  = foldTree f (f x acc) left
-- foldTree f acc (Node left x right) = undefined
-- foldTree f acc tree             = foldr f acc $ inorder tree

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt =
  foldTree undefined undefined undefined

testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree' (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"
