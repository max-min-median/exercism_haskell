module Counting (
    Color(..),
    territories,
    territoryFor
) where

import qualified Data.Set as S
import qualified Data.Array as A
import Data.List (transpose)
import Control.Monad (foldM, forM_)
import Control.Monad.Trans.State (evalState, execState, get, modify', State)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type BoardArray = A.Array Coord Char

flood :: BoardArray -> Coord -> State (S.Set Coord, Bool, Bool) ()
flood board coord
  | not . A.inRange (A.bounds board) $ coord = pure ()
  | here == 'B' = modify' (\(s,b,w) -> (s,True,w))
  | here == 'W' = modify' (\(s,b,w) -> (s,b,True))
  | otherwise = do
    (terr,b,w) <- get
    if coord `S.member` terr then pure () else do
      modify' $ \_ -> (S.insert coord terr,b,w)
      forM_ dirs $ flood board . (coord +.+)
  where
    here = board A.! coord
    dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]

territories :: [String] -> [(S.Set Coord, Maybe Color)]
territories boardStr = reverse $ evalState (go (A.indices board)) S.empty
  where
    board = to2DArray boardStr
    go :: [Coord] -> State (S.Set Coord) [(S.Set Coord, Maybe Color)]
    go = foldM step []
    step acc coord = do
      visited <- get
      if coord `S.member` visited then pure acc
      else case territoryFor_A board coord of
          Just result@(terr,_) -> modify' (S.union terr) >> pure (result: acc)
          Nothing -> modify' (S.insert coord) >> pure acc

territoryFor :: [String] -> Coord -> Maybe (S.Set Coord, Maybe Color)
territoryFor = territoryFor_A . to2DArray

territoryFor_A :: BoardArray -> Coord -> Maybe (S.Set Coord, Maybe Color)
territoryFor_A board coord
  | S.null terr = Nothing
  | otherwise = Just (terr, owner)
  where
    (terr,b,w) = execState (flood board coord) (S.empty, False, False)
    owner
      | b && not w = Just Black
      | w && not b = Just White
      | otherwise = Nothing

(+.+) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) +.+ (c, d) = (a+c, b+d)

to2DArray :: [[a]] -> A.Array Coord a
to2DArray [] = A.listArray ((1, 1), (0, 0)) []
to2DArray xs = A.listArray ((1, 1), (rows, cols)) (concat flipped)
  where
    flipped = transpose xs
    rows = length flipped
    cols = length xs
