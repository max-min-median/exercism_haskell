module WordProblem (answer) where

import Data.Char ( isDigit )
import Data.List ( stripPrefix, foldl' )

answer :: String -> Maybe Integer
answer problem = case stripPrefix "What is " problem of
  Nothing -> Nothing
  Just problem' -> case getNumber problem' of
    Nothing -> Nothing
    Just (n, rest) -> case getOperators rest of
      Nothing -> Nothing
      Just fnList -> Just $ foldl' (flip ($)) n fnList

getOperators :: String -> Maybe [Integer -> Integer]
getOperators "?" = Just []
getOperators s = case stripPrefix "plus " s of
  Just s' -> case getNumber s' of
    Nothing -> Nothing
    Just (n, rest) -> case getOperators rest of
      Nothing -> Nothing
      Just otherOperators -> Just ((+n): otherOperators)
  Nothing -> case stripPrefix "multiplied by " s of
    Just s' -> case getNumber s' of
      Nothing -> Nothing
      Just (n, rest) -> case getOperators rest of
        Nothing -> Nothing
        Just otherOperators -> Just ((*n): otherOperators)
    Nothing -> case stripPrefix "minus " s of
      Just s' -> case getNumber s' of
        Nothing -> Nothing
        Just (n, rest) -> case getOperators rest of
          Nothing -> Nothing
          Just otherOperators -> Just (flip (-) n: otherOperators)
      Nothing -> case stripPrefix "divided by " s of
        Just s' -> case getNumber s' of
          Nothing -> Nothing
          Just (n, rest) -> case getOperators rest of
            Nothing -> Nothing
            Just otherOperators -> Just ((`div` n): otherOperators)
        Nothing -> Nothing

getNumber :: String -> Maybe (Integer, String)
getNumber s
  | numStr == "" = Nothing
  | otherwise = Just (read numStr, dropWhile (== ' ') rest)
  where
    (numStr, rest) = span (\x -> isDigit x || x == '-') s