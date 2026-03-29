module WordProblem (answer) where

import Text.Read ( readMaybe )
import Control.Monad ( guard )

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc x = Just (init x, last x)

answer :: String -> Maybe Integer
answer str = do
  (body, questionMark) <- unsnoc str
  guard $ questionMark == '?'
  case words body of
    "What": "is": x: rest -> do
      n <- readMaybe x
      go n rest
    _                     -> Nothing
  where
    go acc xs = case xs of
      "plus": x: rest             -> step (+) x rest acc
      "minus": x: rest            -> step (-) x rest acc
      "multiplied": "by": x: rest -> step (*) x rest acc
      "divided": "by": x: rest    -> step div x rest acc
      []                           -> Just acc
      _                            -> Nothing
      where
        step op x rest acc' = do
          n <- readMaybe x
          go (acc' `op` n) rest