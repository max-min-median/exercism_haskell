module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep p (x: xs)
  | p x = x: keep p xs
  | otherwise = keep p xs

discard :: (a -> Bool) -> [a] -> [a]
discard p lst = keep (not . p) lst
