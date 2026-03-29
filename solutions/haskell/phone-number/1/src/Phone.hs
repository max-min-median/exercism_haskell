module Phone (number) where

import Data.Ix
import Data.Char (isDigit)

isN :: Char -> Bool
isN = inRange ('2', '9')

isX :: Char -> Bool
isX = isDigit

number :: String -> Maybe String
number xs = removeHeadOne $ filter isDigit xs
  where
    removeHeadOne ('1': rest) = go rest
    removeHeadOne rest = go rest
    go n
      | length n /= 10 = Nothing
      | and $ zipWith ($) [isN, isX, isX, isN, isX, isX, isX, isX, isX, isX] n = Just n
      | otherwise = Nothing