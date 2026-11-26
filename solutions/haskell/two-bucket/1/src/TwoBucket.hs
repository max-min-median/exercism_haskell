module TwoBucket (measure) where

import qualified Data.Set as S
import Data.List (find)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure caps@(c1,c2) target = go 1 (S.singleton (c1,0)) (S.fromList [(c1,0), (0,c2)])
  where
    go :: Int -> S.Set (Int,Int) -> S.Set (Int,Int) -> Maybe (Int, (Int, Int))
    go moves layer visited
      | null layer = Nothing
      | otherwise  = case find (\(a,b) -> a == target || b == target) (S.toList layer) of
        Just (x,y) -> Just (moves, (x,y))
        Nothing    -> let nextLayer   = S.difference (S.fromList (concatMap (neighbours caps) (S.toList layer))) visited
                          nextVisited = S.union visited nextLayer           
                      in  go (moves+1) nextLayer nextVisited

neighbours :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbours (aCap, bCap) (a, b) = filter valid [ (aCap,b), (a,bCap), (0,b), (a,0) 
                                              , let a' = min (a+b) aCap in (a', total-a')
                                              , let b' = min (a+b) bCap in (total-b', b') ]
  where
    total = a + b
    valid (x,y) = x >= 0 && y >= 0 && (x,y) /= (a,b)