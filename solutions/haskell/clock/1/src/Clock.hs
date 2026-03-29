module Clock (addDelta, fromHourMin, toString) where

import Data.List (intercalate)

data Clock = Clock {
  hours :: Int,
  minutes :: Int
} deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock ((h + m `div` 60) `mod` 24) (m `mod` 60)

toString :: Clock -> String
toString x = intercalate ":" $ map (pad . show . ($ x)) [hours, minutes]
  where pad s
          | length s == 2 = s
          | otherwise = "0" ++ s

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm (Clock h m) = fromHourMin (h + dh) (m + dm)