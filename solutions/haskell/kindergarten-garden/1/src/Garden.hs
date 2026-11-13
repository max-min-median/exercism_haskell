module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.Map as M
import Data.List (sort, transpose)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = M.Map String [Plant]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | null chunk = []
  | otherwise  = chunk : chunksOf n rest
  where (chunk,rest) = splitAt n xs

garden :: [String] -> String -> Garden
garden students plants = M.fromList $ zip studentsByName plantsRearranged
  where
    plantsRearranged = map (map plantFromString . concat) . transpose . map (chunksOf 2) . lines $ plants
    studentsByName = sort students

plantFromString :: Char -> Plant
plantFromString xs = case xs of
  'C' -> Clover
  'G' -> Grass
  'R' -> Radishes
  'V' -> Violets
  _   -> error "Bad character in plant string"
  

lookupPlants :: String -> Garden -> [Plant]
lookupPlants = flip (M.!)