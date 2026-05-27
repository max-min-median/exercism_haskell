module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)
import Data.List (uncons)

isbn :: String -> Bool
isbn = go . filter (/= '-')
  where
    go xs = case uncons (reverse xs) of
      Just (end, rest)
        | length rest /= 9               -> False
        | not $ all isDigit rest         -> False
        | not $ end `elem` "0123456789X" -> False
        | otherwise -> (== 0) . (`mod` 11) . sum . zipWith (*) [10,9..1] . map digit $ xs
      Nothing -> False
      where
        digit 'X' = 10
        digit ch = digitToInt ch
