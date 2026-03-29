module Pangram (isPangram) where

import Data.Char (ord, toUpper)
import Data.Bits

isPangram :: String -> Bool
isPangram text =
    let bitset = zeroBits :: Int
        h bs s 26 = True
        h bs "" count = False
        h bs (x:xs) count
            | ch < 0 || ch > 25 = h bs xs count
            | otherwise = h (setBit bs ch) xs (count + if testBit bs ch then 0 else 1)
            where ch = ord (toUpper x) - 65
    in h bitset text 0