module Triplet (tripletsWithSum) where

import Control.Monad (guard)

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum' = do
  a <- [1 .. sum' `div` 3]
  b <- [a+1 .. (a+sum') `div` 2]
  let c = sum' - b - a
  guard $ a*a + b*b == c*c
  pure (a,b,c)