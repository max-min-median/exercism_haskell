module OCR (convert) where

import qualified Data.Map as M
import Data.List (intercalate, transpose)

digitMap :: M.Map [String] Char
digitMap = M.fromList [([" _ ", "| |", "|_|", "   "], '0'),
                       (["   ", "  |", "  |", "   "], '1'),
                       ([" _ ", " _|", "|_ ", "   "], '2'),
                       ([" _ ", " _|", " _|", "   "], '3'),
                       (["   ", "|_|", "  |", "   "], '4'),
                       ([" _ ", "|_ ", " _|", "   "], '5'),
                       ([" _ ", "|_ ", "|_|", "   "], '6'),
                       ([" _ ", "  |", "  |", "   "], '7'),
                       ([" _ ", "|_|", "|_|", "   "], '8'),
                       ([" _ ", "|_|", " _|", "   "], '9')]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs: chunksOf n (drop n xs)

convert :: String -> String
convert xs
  | length rows `mod` 4 /= 0 = error "convert: Number of rows is not a multiple of 4"
  | any ((/= 0) . (`mod` 3) . length) rows = error "convert: Some rows length are not a multiple of 3"
  | otherwise = intercalate "," . map (map convertSingle) $ processedRows
  where
    rows = lines xs
    processedRows = map (transpose . map (chunksOf 3)) . chunksOf 4 $ rows

convertSingle :: [String] -> Char
convertSingle digit = case digitMap M.!? digit of
  Just ch -> ch
  Nothing -> '?'