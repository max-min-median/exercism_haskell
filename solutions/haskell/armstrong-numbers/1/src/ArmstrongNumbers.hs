module ArmstrongNumbers (armstrong) where

import Data.List (unfoldr)
import Data.Tuple (swap)

armstrong :: Integral a => a -> Bool
armstrong x = x == sum (map (^len) digits)
  where
    digits = toDigits x
    len = length digits :: Int

toDigits :: Integral a => a -> [a]
toDigits = unfoldr build
  where
    build 0 = Nothing
    build x = Just (swap (divMod x 10))