module GameOfLife (tick) where

import Data.Array

listTo2DArray :: [[a]] -> Array (Int, Int) a
listTo2DArray xss = listArray ((0, 0), (rMax, cMax)) $ concat xss
  where
    rMax = length xss - 1
    cMax = length (head xss) - 1

tick :: [[Int]] -> [[Int]]
tick xss = [[evolve (r, c) | c <- [0..cMax]] | r <- [0..rMax]]
  where
    rMax = length xss - 1
    cMax = length (head xss) - 1
    grid = listTo2DArray xss
    gridBounds = bounds grid
    evolve (r, c)
      | grid ! (r, c) == 1 = if inRange (3, 4) totalLive then 1 else 0
      | otherwise = if totalLive == 3 then 1 else 0
      where totalLive = sum [if inRange gridBounds (rr, cc) then grid ! (rr, cc) else 0 | rr <- [r-1..r+1], cc <- [c-1..c+1]]