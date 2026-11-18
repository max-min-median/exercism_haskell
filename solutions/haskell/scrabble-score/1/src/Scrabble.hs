module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter ch'
  | ch `elem` "AEIOULNRST" = 1
  | ch `elem` "DG"         = 2
  | ch `elem` "BCMP"       = 3
  | ch `elem` "FHVWY"      = 4
  | ch == 'K'              = 5
  | ch `elem` "JX"         = 8
  | ch `elem` "QZ"         = 10
  | otherwise              = 0
  where
    ch = toUpper ch'

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
