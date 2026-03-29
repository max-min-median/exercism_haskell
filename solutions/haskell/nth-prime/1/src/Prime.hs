module Prime (nth) where

primeTest :: Integer -> [Integer] -> Bool
primeTest x = all (\n -> x `mod` n /= 0)

extendList :: Int -> [Integer] -> [Integer]
extendList desiredLen lst
  | desiredLen <= len = lst
  | otherwise = go (desiredLen - len) lst (head lst + 2)
  where
    len = length lst
    go 0 xs _ = xs
    go n xs x
      | primeTest x xs = go (n - 1) (x: xs) (x + 2)
      | otherwise = go n xs (x + 2)

nth :: Int -> Maybe Integer
nth 1 = Just 2
nth n
  | n <= 0 = Nothing
  | otherwise = Just $ head $ extendList n [3, 2]
