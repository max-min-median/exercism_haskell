module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n = [[numAt (r, c) | c <- [0 .. n-1]] | r <- [0 .. n-1]]
  where
    numAt (r, c)
      | r == layer     = firstNum + c - layer
      | r == n-1-layer = firstNum + 2*(sideLen-1) + n - layer - c - 1
      | c == layer     = firstNum + 3*(sideLen-1) + n - layer - r - 1
      | c == n-1-layer = firstNum + (sideLen-1) + r - layer
      | otherwise      = error "numAt: unexpected error"
      where
        layer = minimum [r, c, n-1-r, n-1-c]
        sideLen = n - 2*layer
        firstNum = n^2 - sideLen^2 + 1