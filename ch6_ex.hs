import Data.List

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = (+) (fromInteger x) (f y)

main :: IO ()
main = print "Hello"
