module Pangram (isPangram) where

import Data.Char (ord, toUpper, isAlpha)
import Data.Bits

isPangram :: String -> Bool
isPangram text =
    let h 0x3ffffff _ = True
        h _ "" = False
        h bs (x:xs)
            | not $ isAlpha x = h bs xs
            | otherwise = h (setBit bs $ ord (toUpper x) - 65) xs
    in h (zeroBits :: Int) text