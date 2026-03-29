module Anagram (anagramsFor) where

import qualified Data.Map as M
import Data.Char (toUpper)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isAnagram xs) xss

isAnagram :: String -> String -> Bool
isAnagram x y = ux /= uy && freqMap ux == freqMap uy
  where (ux, uy) = (map toUpper x, map toUpper y)

freqMap :: String -> M.Map Char Int
freqMap s = M.fromListWith (+) [(x, 1) | x <- s]