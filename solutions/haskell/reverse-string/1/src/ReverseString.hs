module ReverseString (reverseString) where

reverseString :: String -> String
reverseString "" = ""
reverseString s = h "" s
    where h acc "" = acc
          h acc (x:xs) = h (x:acc) xs