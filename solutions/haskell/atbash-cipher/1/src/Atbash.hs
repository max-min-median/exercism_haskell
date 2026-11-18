module Atbash (decode, encode) where

import Data.List
import Data.Char
import Data.Char (isAlphaNum)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | null chunk = []
  | otherwise  = chunk : chunksOf n rest
  where (chunk,rest) = splitAt n xs

decode :: String -> String
decode = map rotate . filter (/= ' ')

rotate :: Char -> Char
rotate x
  | isAlpha x = toEnum . (219 -) . fromEnum $ x
  | otherwise = x

encode :: String -> String
encode = intercalate " " . chunksOf 5 . map (rotate . toLower) . filter isAlphaNum