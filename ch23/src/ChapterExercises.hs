module ChapterExercises where

newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec (State sa) = \s -> snd $ sa s

eval :: State s a -> s -> a
eval (State sa) = \s -> fst $ sa s

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

main :: IO ()
main = do
  print $ runState get "asd"
  print $ runState (put "blah") "woot"
  print $ exec (put "wilma") "asd"
  print $ exec get "asd"
  print $ eval get "asd"
  print $ runState (modify (+1)) 0
  -- print $ runState (modify (+1) >> modify (+1)) 0
