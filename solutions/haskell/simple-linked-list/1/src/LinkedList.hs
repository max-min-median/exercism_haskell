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

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = LinkedList x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = LinkedList

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList xs = go Nil xs
  where
    go acc Nil = acc
    go acc (LinkedList y ys) = go (LinkedList y acc) ys

toList :: LinkedList a -> [a]
toList Nil = []
toList (LinkedList x xs) = x: toList xs