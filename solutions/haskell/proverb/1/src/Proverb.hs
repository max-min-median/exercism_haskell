module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite xss@(x:xs) = intercalate "\n" $ map forWant (zip xss xs) ++ ["And all for the want of a " ++ x ++ "."]
  where forWant (n1, n2) = "For want of a " ++ n1 ++ " the " ++ n2 ++ " was lost."
