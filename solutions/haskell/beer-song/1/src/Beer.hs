module Beer (song) where

import Data.List (intercalate)

stanza :: Int -> String
stanza 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
           \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
stanza n = bottles ++ " on the wall, " ++ bottles
           ++ ".\nTake " ++ (if n == 1 then "it" else "one") ++ " down and pass it around, " ++ nextBottles ++ " on the wall.\n"
  where
    bottles = bottlePhrase n
    nextBottles = bottlePhrase (n-1)

bottlePhrase :: Int -> String
bottlePhrase 0 = "no more bottles of beer"
bottlePhrase n = show n ++ " bottle" ++ (if n == 1 then "" else "s") ++ " of beer"

song :: String
song = intercalate "\n" $ map stanza [99,98..0]