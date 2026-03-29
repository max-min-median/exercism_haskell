module WordCount (wordCount) where

import Data.Char ( isAlphaNum, isAlpha, toLower )
import qualified Data.Map as Map

toWordList :: String -> [String]
toWordList "" = []
toWordList str@(_: xs) = words . map transform $ zip3 (' ': str) str (xs ++ " ") 
  where
    transform (left, mid, right)
      | isAlphaNum mid = toLower mid
      | mid == '\'' && isAlpha left && isAlpha right = '\''
      | otherwise = ' '

wordCount :: String -> [(String, Int)]
wordCount = Map.toAscList . Map.fromListWith (+) . map (\x -> (x, 1)) . toWordList