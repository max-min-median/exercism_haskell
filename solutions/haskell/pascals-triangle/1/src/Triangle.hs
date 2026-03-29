module Triangle (rows) where

triangle :: [[Integer]]
triangle = [1]: go [1]
  where
    go row = newRow: go newRow
      where
        newRow = zipWith (+) (0: row) (row ++ [0])

rows :: Int -> [[Integer]]
rows = (`take` triangle)
