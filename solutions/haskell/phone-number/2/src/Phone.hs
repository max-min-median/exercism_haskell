module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number = dropFirstOne . filter isDigit
  where
    dropFirstOne ('1': rest) = go rest
    dropFirstOne rest = go rest
    go n@[a, _, _, b, _, _, _, _, _, _]
      | all (> '1') [a, b] = Just n
    go _ = Nothing