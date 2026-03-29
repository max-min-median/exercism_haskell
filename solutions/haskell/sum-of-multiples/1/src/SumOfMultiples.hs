module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ foldl merge [] factors where
  merge acc 0 = acc
  merge acc factor = reverse $ go acc [factor, 2 * factor .. limit - 1] [] where
    go l1@(x: xs) l2@(y: ys) newLst
      | x == y = go xs ys (x: newLst)
      | x < y = go xs l2 (x: newLst)
      | x > y = go l1 ys (y: newLst)
    go (x: xs) [] newLst = go xs [] (x: newLst)
    go [] (y: ys) newLst = go ys [] (y: newLst)
    go _ _ newLst = newLst