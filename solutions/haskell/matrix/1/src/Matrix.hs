module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List.Split (chunksOf)
import qualified Data.List as L

newtype Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols = snd . shape

column :: Int -> Matrix a -> Vector a
column n (Matrix x) = V.map (V.! (n-1)) x

flatten :: Matrix a -> Vector a
flatten (Matrix m) = V.concat (V.toList m)

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList

fromString :: Read a => String -> Matrix a
fromString xs = fromList . map (map read . words) . lines $ xs

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) = fromList . chunksOf c . V.toList . flatten

row :: Int -> Matrix a -> Vector a
row n (Matrix m) = m V.! (n-1)

rows :: Matrix a -> Int
rows = fst . shape

shape :: Matrix a -> (Int, Int)
shape (Matrix m)
  | V.length m == 0 = (0, 0)
  | otherwise       = (r, c)
  where
    r = V.length m
    c = V.length (V.head m)

transpose :: Matrix a -> Matrix a
transpose = fromList . L.transpose . toList
  where
    toList (Matrix m) = map V.toList . V.toList $ m
