module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower)

chunk :: Int -> [Char] -> [Char]
chunk n xs = let more [] = []; more ys = ' ':take n ys ++ more (drop n ys) in take n xs ++ more (drop n xs)

encode :: (Int, Int) -> String -> Maybe String
-- E(x) = (a*idx + b) mod m
encode (a, b) plainText
  | gcd' > 1  = Nothing
  | otherwise = Just . chunk 5 . map (affineEncode . toLower) . filter (\ch -> isAlpha ch || isDigit ch) $ plainText
  where
    affineEncode ch
      | isDigit ch = ch
      | otherwise  = let smallA = fromEnum 'a' in toEnum (smallA + (a*(fromEnum ch - smallA) + b) `mod` 26) 
    (gcd',_,_) = egcd a 26

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | gcd' > 1  = Nothing
  | otherwise = Just . map affineDecode . filter (/= ' ') $ cipherText
  where
    affineDecode ch
      | isDigit ch = ch
      | otherwise  = let smallA = fromEnum 'a' in toEnum $ smallA + (mmi * (fromEnum ch - smallA - b)) `mod` 26
    (gcd',(mmi,_),_) = egcd a 26

egcd :: Int -> Int -> (Int, (Int, Int), (Int, Int))
egcd a b = go (1, 0) (0, 1) a b
  where
    go _ (m, n) 0 gcd' =
      let lcm' = a*b `div` gcd' in if m > 0 then (gcd', (m, n), (m - lcm' `div` a, n + lcm' `div` b))
                                          else (gcd', (m + lcm' `div` a, n - lcm' `div` b), (m, n))
    go (m,n) (p,q) x y = let k = y `div` x in go (p-k*m, q-k*n) (m,n) (y - k*x) x