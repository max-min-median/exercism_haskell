module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum . filter ((`any` factors') . divides) $ [1 .. limit-1]
  where
    factors' = filter (/= 0) factors
    divides m n = m `mod` n == 0