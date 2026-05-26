module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

data CustomSet a = CustomSet {toList :: [a]} deriving Show

instance Eq a => Eq (CustomSet a) where
  setA == setB = setA `isSubsetOf` setB && setB `isSubsetOf` setA

instance Foldable CustomSet where
  foldr _ acc (CustomSet []) = acc
  foldr f acc (CustomSet (x:xs)) = f x (foldr f acc (CustomSet xs))

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x = CustomSet . filter (/= x) . toList

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
setA `difference` setB = CustomSet (filter (not . (`member` setB)) (toList setA))

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList = foldr insert empty

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set = if x `member` set then set else CustomSet (x: toList set)

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
setA `intersection` setB = CustomSet (filter (`member` setB) (toList setA))

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
setA `isDisjointFrom` setB = all (not . (`member` setB)) setA

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
setA `isSubsetOf` setB = all (`member` setB) setA

member :: Eq a => a -> CustomSet a -> Bool
member x xs = x `elem` xs

null :: Eq a => CustomSet a -> Bool
null ss = toList ss == []

size :: CustomSet a -> Int
size = length

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = foldr insert setA setB
