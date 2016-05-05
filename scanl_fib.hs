module ScanFib where

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' fun acc values =
  acc : scanl fun (fun acc $ head values) (tail values)
  -- acc : (case values of
           -- x:xs -> scanl fun (fun acc x) xs)

fibs = 1 : scanl (+) 1 fibs
