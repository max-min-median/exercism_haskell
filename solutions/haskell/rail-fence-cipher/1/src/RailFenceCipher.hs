module RailFenceCipher (encode, decode) where

import Data.List

encode :: Int -> String -> String
encode = encodeWith inverse

decode :: Int -> String -> String
decode = encodeWith id

encodeWith :: ([Int] -> [Int]) -> Int -> String -> String
encodeWith f rails xs = map snd . sort $ zip posMap xs
  where
    posMap = f $ makePosMap (length xs) rails

makePosMap :: Int -> Int -> [Int]
makePosMap len rails = map snd . sort $ zip zigzag [0 .. len-1]
  where
    zigzag = cycle ([0 .. rails-1] <> [rails-2, rails-3 .. 1])

inverse :: [Int] -> [Int]
inverse xs = map snd . sort $ zip xs [0..]