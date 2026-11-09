module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (find)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor = go Nothing [maxFactor,maxFactor-1..minFactor]
  where
    go :: Maybe (Integer, [(Integer, Integer)]) -> [Integer] -> Maybe (Integer, [(Integer, Integer)])
    go acc []                        = acc
    go Nothing (x:xs)                = go (fmap (:[]) <$> findPalindrome x minFactor) xs
    go acc@(Just (best,bestPairs)) (x:xs)
      | x * x < best = acc
      | otherwise    = case findPalindrome x (best `div` x) of
        Nothing           -> go acc xs
        Just (best',bestPair')
          | best' == best -> go (Just (best,bestPair':bestPairs)) xs
          | best' > best  -> go (Just (best',[bestPair'])) xs
          | otherwise     -> go acc xs
    findPalindrome a b = (\p -> (p,(a, p `div` a))) <$> find isPalindrome [a*a, a*(a-1) .. a*b]

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor = go Nothing [minFactor..maxFactor]
  where
    go :: Maybe (Integer, [(Integer, Integer)]) -> [Integer] -> Maybe (Integer, [(Integer, Integer)])
    go acc []                        = acc
    go Nothing (x:xs)                = go (fmap (:[]) <$> findPalindrome x maxFactor) xs
    go acc@(Just (best,bestPairs)) (x:xs)
      | x * x > best = acc
      | otherwise    = case findPalindrome x (best `div` x) of
        Nothing           -> go acc xs
        Just (best',bestPair')
          | best' == best -> go (Just (best,bestPair':bestPairs)) xs
          | best' < best  -> go (Just (best',[bestPair'])) xs
          | otherwise     -> go acc xs
    findPalindrome a b = (\p -> (p,(a, p `div` a))) <$> find isPalindrome [a*a, a*(a+1) .. a*b]


isPalindrome :: Integer -> Bool
isPalindrome n = let str = show n in str == reverse str