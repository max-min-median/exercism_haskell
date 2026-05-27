module Yacht (yacht, Category(..)) where

import Data.List (sort)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht cat xs = if length xs /= 5 then 0 else case cat of
  Ones      -> sum $ filter (== 1) xs
  Twos      -> sum $ filter (== 2) xs
  Threes    -> sum $ filter (== 3) xs
  Fours     -> sum $ filter (== 4) xs
  Fives     -> sum $ filter (== 5) xs
  Sixes     -> sum $ filter (== 6) xs
  FullHouse -> case sort xs of
    [a,b,c,d,e] -> if a == c && c < d && d == e || a == b && b < c && c == e then sum xs else 0
    _           -> 0
  FourOfAKind -> case sort xs of
    [a,b,c,d,e] -> if a == d || b == e then 4*c else 0
    _           -> 0
  LittleStraight -> case sort xs of
    [1,2,3,4,5] -> 30
    _           -> 0
  BigStraight -> case sort xs of
    [2,3,4,5,6] -> 30
    _           -> 0
  Choice -> sum xs
  Yacht -> case xs of
    [a,_,_,_,_] -> if all (== a) xs then 50 else 0
    _           -> 0