module Minesweeper (annotate) where

import qualified Data.Map.Strict as M
import Data.Char (chr)

dirs8 :: [(Int, Int)]
dirs8 = [(dr, dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], dr /= 0 || dc /= 0]

mineMap :: [String] -> M.Map (Int, Int) Int
mineMap grid = go grid (0, 0) M.empty
  where
    go [] _ res = res
    go ([]: as) (r, _) res = go as (r + 1, 0) res
    go ((x: xs): as) (r, c) res = go (xs: as) (r, c+1) res2 where
      res2
        | x == '*' = foldl (\m (dr, dc) -> M.insertWith (+) (r+dr, c+dc) 1 m) res dirs8
        | otherwise = res

annotate :: [String] -> [String]
annotate strList = map annotateRow $ zip [0..] strList
  where
    mines = mineMap strList
    annotateRow (r, row) = map charChanger $ zip [0..] row
      where
        charChanger (c, ch)
          | ch == '*' = '*'
          | otherwise = case M.lookup (r, c) mines of
            Just x -> chr (48 + x)
            Nothing -> ' '