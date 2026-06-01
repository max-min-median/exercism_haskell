{-# LANGUAGE TupleSections #-}

module Alphametics (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (transpose)
import Data.List.Split (splitOn)
import Control.Applicative (asum)
import Data.Maybe (listToMaybe, catMaybes)

type Layer = M.Map Char Int
type Carry = Int
type DigitsLeft = S.Set Int
type Assignment = M.Map Char Int

solve :: String -> Maybe [(Char, Int)]
solve puzzle = case parse puzzle of
  Right (parsed, firstLetters) -> M.toList <$> assignLayer parsed 0 M.empty (S.fromList [0..9]) 
    where
      assignLayer :: [Layer] -> Carry -> Assignment -> DigitsLeft -> Maybe Assignment
      assignLayer [] carry assignment _ = if carry == 0 then Just assignment else Nothing
      assignLayer (l:ls) carry assignment digitsLeft = try layerChars carry assignment digitsLeft
        where
          layerList = M.toList l
          layerChars = map fst layerList
          try [] carry' assignment' digitsLeft' =
            let total = sum (map (\(key, count) -> assignment' M.! key * count) layerList) + carry'
            in  if total `mod` 10 == 0 then assignLayer ls (total `div` 10) assignment' digitsLeft' else Nothing
          try (toAssign: rest) carry' assignment' digitsLeft' =
            if toAssign `M.member` assignment' then try rest carry' assignment' digitsLeft' else
            asum [try rest carry' (M.insert toAssign x assignment') (S.delete x digitsLeft')
                | x <- S.toList digitsLeft'
                , x /= 0 || not (toAssign `S.member` firstLetters)
                ] 
  _                            -> Nothing

parse :: String -> Either String ([Layer], S.Set Char)
parse xs = case splitOn " == " xs of
  [lhs, rhs] -> Right (map (M.fromListWith (+)) . transpose $ lhs' ++ [rhs'], firstLetters)
    where
      lhsWords = splitOn " + " lhs
      firstLetters = S.fromList . catMaybes . map listToMaybe $ (rhs: lhsWords)
      lhs' = map (map (,1) . reverse) lhsWords
      rhs' = reverse . map (,-1) $ rhs
  _          -> Left "Could not find '==' in string"
