module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify num
  | num <= 0 = Nothing
  | aliquotSum > num = Just Abundant
  | aliquotSum == num = Just Perfect
  | otherwise = Just Deficient
  where aliquotSum = sum $ filter (\x -> num `mod` x == 0) [1 .. num-1]