module Counting (
    Color(..),
    territories,
    territoryFor
) where

import qualified Data.Set as S
import qualified Data.Array as A
import Data.List (transpose)
import Control.Monad
import Control.Monad.Trans.State

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
      forM_ dirs $ flood board . (coord <+.+>)
  where
    here = board A.! coord
    dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]

territories :: [String] -> [(S.Set Coord, Maybe Color)]
territories boardStr = evalState (go (A.indices board)) S.empty
  where
    board = to2DArray boardStr
    go :: [Coord] -> State (S.Set Coord) [(S.Set Coord, Maybe Color)]
    go [] = pure []
    go (coord:cs) = do
      visited <- get
      if coord `S.member` visited then go cs else do
        modify' (S.insert coord)
        case territoryFor_A board coord of
          Just result@(terr,_) -> do 
            modify' (S.union terr)
            moreResults <- go cs
            pure (result: moreResults)
          Nothing -> go cs

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

(<+.+>) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) <+.+> (c, d) = (a+c, b+d)

to2DArray :: [[a]] -> A.Array Coord a
to2DArray [] = A.listArray ((1, 1), (0, 0)) []
to2DArray xs = A.listArray ((1, 1), (rows, cols)) (concat flipped)
  where
    flipped = transpose xs
    rows = length flipped
    cols = length xs

testBoard :: [[Char]]
testBoard = ["                   ", --  1
             "  BBB              ", --  2
             " B   B  BB         ", --  3
             " B    BB  B        ", --  4
             " B        B        ", --  5
             " B      BB         ", --  6
             " B     B     WW    ", --  7
             " B   B  B   W  W  W", --  8
             "  BBB B  B  W  W W ", --  9
             "       BB  W    W  ", -- 10
             "            W      "] -- 11
            --0        1         
            --1234567890123456789