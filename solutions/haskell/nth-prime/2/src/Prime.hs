module Prime (nth) where

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p: xs) = p: (sieve $ filter ((/= 0) . flip mod p) xs)

nth :: Int -> Maybe Integer
nth 1 = Just 2
nth n
  | n <= 0 = Nothing
  | otherwise = Just (primeList !! (n - 1))
  where primeList = sieve [2..]