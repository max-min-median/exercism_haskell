module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Dir = L | R deriving (Eq, Show)

data Zipper a = Zipper { toTree :: BinTree a, focusPath :: [Dir] } deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- toTree :: Zipper a -> BinTree a
-- toTree zipper = error "You need to implement this function."

value :: Zipper a -> a
value = btValue . getFocus

getFocus :: Zipper a -> BinTree a 
getFocus (Zipper tree path) = go path (Just tree)
  where
    go _ Nothing           = error "Unreachable"
    go [] (Just t)         = t 
    go (dir:dirs) (Just t) = go dirs (dirFn dir t)

dirFn :: Dir -> BinTree a -> Maybe (BinTree a)
dirFn L = btLeft
dirFn R = btRight 

goDir :: Dir -> Zipper a -> Maybe (Zipper a)
goDir dir zipper = dirFn dir (getFocus zipper) >> pure (zipper {focusPath = focusPath zipper <> [dir]})

left :: Zipper a -> Maybe (Zipper a)
left = goDir L

right :: Zipper a -> Maybe (Zipper a)
right = goDir R

up :: Zipper a -> Maybe (Zipper a)
up zipper = unsnoc (focusPath zipper) >>= \(newPath,_) -> pure (zipper { focusPath = newPath })

setValue :: a -> Zipper a -> Zipper a
setValue x z@(Zipper tree path) = z { toTree = go path (Just tree) }
  where
    go _ Nothing         = error "Unreachable"
    go [] (Just t)       = t { btValue = x }
    go (L:dirs) (Just t) = t { btLeft = Just $ go dirs (btLeft t) }
    go (R:dirs) (Just t) = t { btRight = Just $ go dirs (btRight t) }

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft x z@(Zipper tree path) = z { toTree = go path (Just tree) }
  where
    go _ Nothing         = error "Unreachable"
    go [] (Just t)       = t { btLeft = x }
    go (L:dirs) (Just t) = t { btLeft = Just $ go dirs (btLeft t) }
    go (R:dirs) (Just t) = t { btRight = Just $ go dirs (btRight t) }

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight x z@(Zipper tree path) = z { toTree = go path (Just tree) }
  where
    go _ Nothing         = error "Unreachable"
    go [] (Just t)       = t { btRight = x }
    go (L:dirs) (Just t) = t { btLeft = Just $ go dirs (btLeft t) }
    go (R:dirs) (Just t) = t { btRight = Just $ go dirs (btRight t) }

-- no 'unsnoc' in exercism...
unsnoc :: [a] -> Maybe ([a], a)
unsnoc []  = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = (\(a,b) -> (x:a,b)) <$> unsnoc xs