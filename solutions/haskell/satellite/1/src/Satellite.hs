module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List

treeFromTraversals :: (Ord a, Show a) => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder' inorder'
  | any hasRepeats [preorder', inorder'] || any null [preorder', inorder'] = Nothing
  | otherwise = go preorder' inorder'
  where
    hasRepeats x = length x /= length (nub x)
    go (p:ps) inorder = do
      idx <- findIndex (== p) inorder
      left <- go (take idx ps) (take idx inorder)
      right <- go (drop idx ps) (drop (idx+1) inorder)
      pure $ Branch left p right

    go [] [] = Just Leaf
    go _ _ = Nothing

--   a
--  / \
-- i   x
--    / \
--   f   r

-- For example the pre-order traversal of this tree is [a, i, x, f, r].
-- The in-order traversal of this tree is [i, a, f, x, r]

{-
Algorithm:
1. Take first node of pre-order list.
2. Use that to split in-order list into left and right sublists (subtrees).
3. Split tail of pre-order list according to the same number of elements in the sublists in step 2. 
4. Recurse.
-}