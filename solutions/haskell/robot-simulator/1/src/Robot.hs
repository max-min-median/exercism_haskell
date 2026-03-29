module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

import Data.List (foldl')

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Bounded, Enum)

data Robot = Robot {
  bearing :: Bearing,
  coordinates :: (Integer, Integer)
}

-- bearing :: Robot -> Bearing
-- bearing robot = error "You need to implement this function."

-- coordinates :: Robot -> (Integer, Integer)
-- coordinates robot = error "You need to implement this function."

turnRight :: Robot -> Robot
turnRight (Robot bearing' coord) = Robot (if bearing' == maxBound then minBound else succ bearing') coord

turnLeft :: Robot -> Robot
turnLeft (Robot bearing' coord) = Robot (if bearing' == minBound then maxBound else pred bearing') coord

advance :: Robot -> Robot
advance (Robot bearing' (x, y)) = case bearing' of
  North -> Robot bearing' (x, y+1)
  East  -> Robot bearing' (x+1, y)
  South -> Robot bearing' (x, y-1)
  West  -> Robot bearing' (x-1, y)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot instructions = foldl' step robot instructions
  where
    step r ch = case ch of
      'L' -> turnLeft r
      'R' -> turnRight r
      'A' -> advance r
      _   -> error $ "Unexpected character in input: '" ++ ch: "'"
