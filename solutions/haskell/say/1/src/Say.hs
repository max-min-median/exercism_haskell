module Say (inEnglish) where

import Data.List

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 = Nothing
  | n == 0 = Just "zero"
  | otherwise = intercalate " " <$> go n thousandsNames
  where
    thousandsNames = ["", " thousand", " million", " billion"]
    go 0 _      = Just []
    go _ []     = Nothing
    go x (a:as) = let (q,r) = x `divMod` 1000
                  in  go q as <> ((\s -> [s <> a]) <$> subThousandInEnglish r)

subThousandInEnglish :: Integer -> Maybe String
subThousandInEnglish n
  | n < 20 = case n of
    0  -> Nothing
    1  -> Just "one"
    2  -> Just "two"
    3  -> Just "three"
    4  -> Just "four"
    5  -> Just "five"
    6  -> Just "six"
    7  -> Just "seven"
    8  -> Just "eight"
    9  -> Just "nine"
    10 -> Just "ten"
    11 -> Just "eleven"
    12 -> Just "twelve"
    13 -> Just "thirteen"
    14 -> Just "fourteen"
    15 -> Just "fifteen"
    16 -> Just "sixteen"
    17 -> Just "seventeen"
    18 -> Just "eighteen"
    19 -> Just "nineteen"
    _  -> error "Unreachable"
  | n < 100 =
    let tens = case n `div` 10 of
                 2  -> Just "twenty"
                 3  -> Just "thirty"
                 4  -> Just "forty"
                 5  -> Just "fifty"
                 6  -> Just "sixty"
                 7  -> Just "seventy"
                 8  -> Just "eighty"
                 9  -> Just "ninety"
                 _  -> error "Unreachable"
        ones = subThousandInEnglish (n `mod` 10)
    in  tens <> (('-':) <$> ones)
  | otherwise =
    let hundreds = subThousandInEnglish (n `div` 100)
        rest = subThousandInEnglish (n `mod` 100)
    in  hundreds <> Just " hundred" <> ((' ':) <$> rest)
