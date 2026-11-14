module Series (slices) where

import Data.List
import Data.Char

slices :: Integral a => a -> String -> [[Int]]
slices n' xs'
  | length start == n = scanl' step start (drop n xs)
  | otherwise         = []
  where
    start = take n xs
    xs = map digitToInt xs'
    n = fromIntegral n'
    step b a = drop 1 (b <> [a])