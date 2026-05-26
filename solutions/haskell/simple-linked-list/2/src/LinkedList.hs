module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = LinkedList {datum :: a, next :: LinkedList a}
                  | Nil
  deriving (Eq, Show)

instance Foldable LinkedList where
    foldr _ acc Nil = acc
    foldr f acc (LinkedList x xs) = f x (foldr f acc xs)

fromList :: [a] -> LinkedList a
fromList = foldr LinkedList Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = LinkedList

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl (flip new) Nil

toList :: LinkedList a -> [a]
toList = foldr (:) []