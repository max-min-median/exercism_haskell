module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just (go n)
  where go n = if n == 1 then 0 else 1 + go (if odd n then 3 * n + 1 else n `div` 2)