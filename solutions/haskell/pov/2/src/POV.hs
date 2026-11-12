module POV (fromPOV, tracePathBetween) where

import Data.Tree
import Data.Foldable

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = do
  path <- dfs x tree
  pure $ foldl' pullup tree path
  where
    pullup t (Nothing,_) = t
    pullup t (Just i,_) = case splitAt i (subForest t) of
      (front, c:cs) -> c { subForest = subForest c <> [t { subForest = front <> cs }] }
      _             -> error "Unreachable"

dfs :: Eq a => a -> Tree a -> Maybe [(Maybe Int, a)]
dfs x (Node val children)
  | x == val  = Just [(Nothing, x)]
  | otherwise = fmap (\(i, path) -> (Just i, val):path). asum . zipWith (fmap . (,)) [0..] . map (dfs x) $ children

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  t <- fromPOV from tree
  path <- dfs to t
  pure $ map snd path