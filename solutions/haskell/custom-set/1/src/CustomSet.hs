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
  setA == setB = toList (setA `difference` setB) == [] && toList (setB `difference` setA) == [] 

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x = CustomSet . filter (not . (== x)) . toList

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet as) setB = CustomSet (filter (not . (`member` setB)) as)

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList = foldr insert empty

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x set = if x `member` set then set else CustomSet (x: toList set)

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet as) setB = CustomSet (filter (`member` setB) as)

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA = null . intersection setA

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA = null . difference setA

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = any (== x) xs

null :: CustomSet a -> Bool
null (CustomSet []) = True
null _              = False

size :: CustomSet a -> Int
size = length . toList

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union setA (CustomSet bs) = foldr insert setA bs
