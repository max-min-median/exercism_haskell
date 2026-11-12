module POV (fromPOV, tracePathBetween) where

import Data.Tree
import Data.Foldable

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = do
  path <- dfs x tree
  pure $ foldl' pullup tree path
  where
    pullup t (-1,_) = t
    pullup t@(Node _ children) (i,_) = case splitAt i children of
      (front, c:cs) -> c { subForest = subForest c <> [t { subForest = front <> cs }] }
      _             -> error "Unreachable"

dfs :: Eq a => a -> Tree a -> Maybe [(Int,Tree a)]
dfs x tree@(Node val children)
  | x == val  = Just [(-1,tree)]
  | otherwise = fmap (\(i, path) -> (i,tree):path). asum . zipWith (fmap . (,)) [0..] . map (dfs x) $ children

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  t <- fromPOV from tree
  path <- dfs to t
  pure $ map (rootLabel . snd) path
