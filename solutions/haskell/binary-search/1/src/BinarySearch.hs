module BinarySearch (find) where

import Data.Array ((!), bounds, Array)

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x = go lo' hi'
  where
    (lo', hi') = bounds arr
    go lo hi
      | lo > hi = Nothing
      | x == midVal = Just mid
      | x < midVal = go lo (mid-1)
      | otherwise = go (mid+1) hi
      where
        mid = (lo + hi) `div` 2
        midVal = arr ! mid
