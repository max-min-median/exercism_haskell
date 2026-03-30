module Matrix (saddlePoints) where

import Data.Array (Array, Ix, bounds, range, rangeSize, (!))
import Data.List (foldl')


getMinOrMax :: (Ix a, Ix b, Ord e) => (e -> e -> Bool) -> Array (a, b) e -> [(a, b)] -> [(a, b)]
getMinOrMax cmp arr ixs = reverse $ foldl' step [] ixs
  where
    step [] ix = [ix]
    step as@(a: _) ix
      | (arr ! ix) `cmp` (arr ! a) = [ix]
      | (arr ! ix) == (arr ! a) = ix: as
      | otherwise = as


saddlePoints :: (Ix a, Ix b, Ord e) => Array (a, b) e -> [(a, b)]
saddlePoints matrix
  | numRows <= numCols = filter (\x -> x `elem` getMinOrMax (<) matrix (col $ snd x)) rowMaxes
  | otherwise = filter (\x -> x `elem` getMinOrMax (>) matrix (row $ fst x)) colMins
  where  
    ((rMin, cMin), (rMax, cMax)) = bounds matrix
    numRows = rangeSize (rMin, rMax)
    numCols = rangeSize (cMin, cMax)
    row r = range ((r, cMin), (r, cMax))
    col c = range ((rMin, c), (rMax, c))
    rowMaxes = concatMap (getMinOrMax (>) matrix) (map row $ range (rMin, rMax))
    colMins = concatMap (getMinOrMax (<) matrix) (map col $ range (cMin, cMax))