module OCR (convert) where

import qualified Data.Map as M
import Data.List (intercalate)

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

inp :: String
inp = unlines [ "    _  _ "
              , "  | _| _|"
              , "  ||_  _|"
              , "         "
              , "    _  _ "
              , "|_||_ |_ "
              , "  | _||_|"
              , "         "
              , " _  _  _ "
              , "  ||_||_|"
              , "  ||_| _|"
              , "         " ]

convert :: String -> String
convert xs
  | length rows `mod` 4 /= 0 = error "convert: Number of rows is not a multiple of 4"
  | any ((/= 0) . (`mod` 3) . length) rows = error "convert: Some rows length are not a multiple of 3"
  | otherwise = intercalate "," $ go rows
  where
    rows = lines xs
    go [] = []
    go ys = convertRow (take 4 ys): go (drop 4 ys)

convertRow :: [String] -> String
convertRow [] = ""
convertRow xss@(x: _)
  | let rowLen = length x in any ((/= rowLen) . length) xss = error "convertRows: rows are not the same length!"
  | otherwise = go xss
  where
    go ("":_) = ""
    go ys@(_:_) = convertSingle (map (take 3) ys) : go (map (drop 3) ys)
    go _ = error "convertRow: Unexpected branch hit. Check input."

convertSingle :: [String] -> Char
convertSingle digit = case digitMap M.!? digit of
  Just ch -> ch
  Nothing -> '?'