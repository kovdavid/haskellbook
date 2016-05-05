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
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = (inorder left) ++ [x] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = (postorder left) ++ (postorder right) ++ [x]

testTree  = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

-- foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
-- foldTree f acc t = helper t acc
  -- where helper Leaf = id
        -- helper (Node l z r) = helper r . f z . helper l

-- foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
-- foldTree' _ acc Leaf = acc
-- foldTree' f acc (Node left x right) =
  -- let accLeft = foldTree' f acc left
      -- accNode = f x accLeft
  -- in  foldTree' f accNode right

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc tree = foldr f acc $ inorder tree

foldTree' :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree' _ acc Leaf = acc
foldTree' f acc (Node left x right) =
  f x (foldTree' f acc left) (foldTree' f acc right)

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt =
  foldTree' (\x l r -> Node l (f x) r) Leaf bt

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree' (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
-- mapTree _ Leaf = Leaf
-- mapTree f (Node left a right) =
  -- Node (mapTree f left) (f a) (mapTree f right)
