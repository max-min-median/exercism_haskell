module Queens (boardString, canAttack) where

import Data.List ( intersperse )

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [intersperse ' ' [ch (r, c) | c <- [0..7]] | r <- [0..7]]
  where
    ch coord
      | Just coord == white = 'W'
      | Just coord == black = 'B'
      | otherwise = '_'

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) = r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)