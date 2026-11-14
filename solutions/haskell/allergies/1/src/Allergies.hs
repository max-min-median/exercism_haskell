module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.List (unfoldr)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum)

allergies :: Int -> [Allergen]
allergies score = map snd . filter ((== 1) . fst) $ zip indicatorList [Eggs .. Cats]
  where
    indicatorList = unfoldr binDigit score
    binDigit 0 = Nothing
    binDigit x = let (q,r) = divMod x 2 in Just (r,q)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen = elem allergen . allergies
