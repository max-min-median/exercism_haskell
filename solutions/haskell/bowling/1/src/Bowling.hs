module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)


getRoll :: Int -> [Int] -> Either BowlingError Int
getRoll ix (x:_) = if 0 <= x && x <= 10 then Right x else Left (InvalidRoll ix x)
getRoll _ [] = Left IncompleteGame


getFrame :: Int -> [Int] -> Either BowlingError Int
getFrame ix xs = do
  first <- getRoll ix xs
  second <- getRoll (ix+1) (drop 1 xs)
  if first + second < 10 then pure $ first + second
  else if first /= 10 && first + second > 10 then Left (InvalidRoll (ix+1) second)
  else do  -- either spare or strike
    third <- getRoll (ix+2) (drop 2 xs)
    if first == 10 && second /= 10 && second + third > 10 then Left (InvalidRoll (ix+2) third)
    else pure $ first + second + third


score :: [Int] -> Either BowlingError Int
score = go 0 0
  where
    go :: Int -> Int -> [Int] -> Either BowlingError Int
    go 10 _ [] = Right 0
    go 10 ix (x:_) = Left (InvalidRoll ix x)
    go frame ix xs@(x:_) = do
      frameScore <- getFrame ix xs
      let skip = if frame == 9 && frameScore >= 10 then 3
                 else if x == 10 then 1 else 2
      (+) <$> pure frameScore <*> go (frame+1) (ix+skip) (drop skip xs)
    go _ _ _ = Left IncompleteGame