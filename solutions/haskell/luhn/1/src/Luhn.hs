module Luhn (isValid) where

import Data.Char (ord, isSpace)

isValid :: String -> Bool
isValid s
  | length cleaned <= 1 = False
  | otherwise = (== 0) . (`mod` 10) . fst . foldr step (0, 0) $ cleaned
  where cleaned = filter (not . isSpace) s
        step ch (acc, 0) = (acc + ord ch - 48, 1)
        step ch (acc, 1) = (acc + if twice >= 10 then twice - 9 else twice, 0)
          where twice = 2 * (ord ch - 48)