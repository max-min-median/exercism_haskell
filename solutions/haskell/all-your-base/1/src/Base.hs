module Base (Error(..), rebase) where

import Data.List (unfoldr, foldl')
import Data.Tuple (swap)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2  = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | otherwise = reverse . unfoldr fromDecimal <$> dec
  where
    dec = foldl' toDecimal (Right (fromInteger 0)) inputDigits
    toDecimal acc digit
      | digit >= inputBase || digit < 0 = Left (InvalidDigit digit)
      | otherwise                       = ((+ digit) . (* inputBase)) <$> acc
    fromDecimal 0 = Nothing
    fromDecimal x = Just (swap (divMod x outputBase))