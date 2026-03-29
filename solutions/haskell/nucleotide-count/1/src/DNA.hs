module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as M
import Data.Maybe (fromJust)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

toNucleoTide :: Char -> Maybe Nucleotide
toNucleoTide 'A' = Just A
toNucleoTide 'C' = Just C
toNucleoTide 'G' = Just G
toNucleoTide 'T' = Just T
toNucleoTide _ = Nothing

nucleotideCounts :: String -> Either String (M.Map Nucleotide Int)
nucleotideCounts s
    | lst == Nothing = Left "Error"
    | otherwise = Right $ M.union (M.fromListWith (+) [(x, 1) | x <- fromJust lst]) (M.fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
    where lst = traverse toNucleoTide s