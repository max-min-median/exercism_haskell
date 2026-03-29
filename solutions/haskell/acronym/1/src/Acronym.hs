module Acronym (abbreviate) where

import Data.Char ( isLower, isUpper, toUpper )

abbreviate :: String -> String
abbreviate s = reverse $ go (filter (/= '\'') s) "" 0
  where
    go "" acc _ = acc
    go (x: xs) acc prevLvl
      | lvl > prevLvl = go xs (toUpper x: acc) lvl
      | otherwise = go xs acc lvl
      where lvl = if isLower x then 1 else if isUpper x then 2 else 0