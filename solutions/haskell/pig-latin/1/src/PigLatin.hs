module PigLatin (translate) where

import Data.List
import Data.Char

translate :: String -> String
translate = intercalate " " . map transWord . words

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou") . toLower

transWord :: String -> String
transWord "" = ""
transWord xs@(x:_)
  | isVowel x || "xr" `isPrefixOf` xs || "yt" `isPrefixOf` xs = xs <> "ay"
  | not (null consonantsUpToY) && "y" `isPrefixOf` yAndAfter = yAndAfter <> consonantsUpToY <> "ay"
  | "q" `isSuffixOf` consonants && "u" `isPrefixOf` afterConsonants = drop 1 afterConsonants <> init consonants <> "quay"
  | otherwise = afterConsonants <> consonants <> "ay"
  where
    (consonantsUpToY,yAndAfter) = break (\x -> isVowel x || x == 'y') xs
    (consonants,afterConsonants) = break isVowel xs