module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = go n primes
  where
    go 1 _      = []
    go x (p:ps)
      | x `mod` p == 0 = p : go (x `div` p) (p:ps)
      | otherwise      = go x ps
    go _ _ = error "Unreachable"

primes :: [Integer]
primes = 2 : [ x | x <- [3,5..], all ((/= 0) . mod x) (takeWhile (< root x) primes) ]
  where
    root = floor . sqrt . fromIntegral