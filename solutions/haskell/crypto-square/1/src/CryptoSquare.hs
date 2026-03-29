module CryptoSquare (encode) where

import Data.Char (toLower, isSpace, isPunctuation)

encode :: String -> String
encode s
  | c == 0 = ""
  | otherwise = unwords . transpose . chunk c $ cleaned ++ replicate remainder ' '
  where cleaned = clean s
        len = length cleaned
        c = ceiling $ sqrt (fromIntegral len :: Float)
        remainder = if c == 0 then 0 else (c - len `mod` c) `mod` c

clean :: String -> String
clean = map toLower . filter (not. or . sequence [isSpace, isPunctuation])

chunk :: Int -> String -> [String]
chunk _ "" = []
chunk n xs = take n xs: chunk n (drop n xs)

transpose :: [[a]] -> [[a]]
transpose xs = case getRow xs of
  (Nothing, _) -> []
  (Just r, rest) -> r: transpose rest

getRow :: [[a]] -> (Maybe [a], [[a]])
getRow xs = case result of (Nothing, _) -> result
                           (Just x, y) -> (Just $ reverse x, reverse y)
  where go t@(Nothing, _) _ = t
        go _ [] = (Nothing, xs)
        go (Just as, bs) (c: cs) = (Just (c: as), cs: bs)
        result = foldl go (Just [], []) xs