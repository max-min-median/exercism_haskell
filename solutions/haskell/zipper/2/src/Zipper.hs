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

data Ctx a = Ctx a (Either (Maybe (BinTree a)) (Maybe (BinTree a))) deriving (Eq, Show)

data Zipper a = Zipper (BinTree a) [Ctx a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree z@(Zipper t _) = case up z of
  Nothing -> t
  Just z' -> toTree z'

value :: Zipper a -> a
value (Zipper t _) = btValue t

left :: Zipper a -> Maybe (Zipper a)
left (Zipper t c) = Zipper <$> btLeft t <*> Just (Ctx (btValue t) (Right (btRight t)) : c)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper t c) = Zipper <$> btRight t <*> Just (Ctx (btValue t) (Left (btLeft t)) : c)

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ [])     = Nothing
up (Zipper t (c:cx)) = case c of
  Ctx p (Left sib)  -> Just $ Zipper (BT p sib (Just t)) cx
  Ctx p (Right sib) -> Just $ Zipper (BT p (Just t) sib) cx

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper t c) = Zipper (t { btValue = x }) c

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft t' (Zipper t c) = Zipper (t { btLeft = t' }) c

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight t' (Zipper t c) = Zipper (t { btRight = t' }) c