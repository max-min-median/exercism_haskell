module Series (Error(..), largestProduct) where

import Data.Char (digitToInt)
import Data.List.Split

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits = do
  nums <- traverse charToInt digits
  if size < 0 || size > length nums then Left InvalidSpan
  else pure . maximum . map product . divvy size 1 $ nums

charToInt :: Char -> Either Error Integer
charToInt ch
  | ch >= '0' && ch <= '9' = Right (fromIntegral (digitToInt ch))
  | otherwise              = Left (InvalidDigit ch)