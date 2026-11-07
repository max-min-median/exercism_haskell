module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | p + q <= r       = Illegal
  | p == q && q == r = Equilateral
  | p == q || q == r = Isosceles
  | otherwise        = Scalene
  where
    (p,q,r) = case sort [a,b,c] of
      [x,y,z] -> (x,y,z)
      _       -> error "Unreachable"
      
