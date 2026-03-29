module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = BST a (BST a) (BST a) | Empty deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (BST _ x _) = Just x

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (BST _ _ x) = Just x

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (BST x _ _) = Just x

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (BST val left right)
  | x <= val = BST val (insert x left) right
  | otherwise = BST val left (insert x right)

singleton :: a -> BST a
singleton x = BST x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (BST val left right) = toList left ++ [val] ++ toList right
