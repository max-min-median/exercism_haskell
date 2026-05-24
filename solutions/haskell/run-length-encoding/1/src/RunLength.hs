module RunLength (decode, encode) where

import Data.List (group, uncons)
import Data.List.Split (keepDelimsR, split, whenElt)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

decode :: String -> String
decode = concatMap expand . filter (not . null) . split (keepDelimsR (whenElt (not . isDigit)))
  where
    expand xs = fromMaybe "" $ do
      (numStr, ch) <- unsnoc xs
      let num = fromMaybe 1 $ readMaybe numStr
      pure $ replicate num ch

encode :: String -> String
encode text = concatMap shorten $ group text
  where
    shorten xs@(x:_:_) = show (length xs) ++ [x]
    shorten ys = ys