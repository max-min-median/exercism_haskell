module Triangle (rows) where

triangle :: [[Integer]]
triangle = iterate nextRow [1]
  where
    -- nextRow row = zipWith (+) (0: row) (row ++ [0])
    nextRow [] = [1]
    nextRow (x: xs) = x: go x xs
      where
        go a (b: bs) = (a + b): go b bs
        go a [] = [a]

rows :: Int -> [[Integer]]
rows n = take n triangle