module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = go ""
    where go acc "" = Right (reverse acc)
          go acc (x:xs)
            | x `elem` "ACGT" = go (convert x: acc) xs
            | otherwise = Left x
            where convert 'G' = 'C'
                  convert 'C' = 'G'
                  convert 'T' = 'A'
                  convert 'A' = 'U'