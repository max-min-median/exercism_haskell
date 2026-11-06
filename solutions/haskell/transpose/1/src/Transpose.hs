module Transpose (transpose) where

import Data.List (dropWhileEnd)
import Data.Maybe (listToMaybe, fromMaybe)

transpose :: [String] -> [String]
transpose xss
  | null col  = []
  | otherwise = col : transpose (map (drop 1) xss)
  where
    col = map (fromMaybe ' ') . dropWhileEnd (== Nothing) . map listToMaybe $ xss