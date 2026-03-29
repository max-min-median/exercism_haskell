module Darts (score) where

score :: Float -> Float -> Int
score x y
    | d <= 1 = 10
    | d <= 25 = 5
    | d <= 100 = 1
    | otherwise = 0
    where d = x ^ 2 + y ^ 2
