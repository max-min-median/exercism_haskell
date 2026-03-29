module WordSearch (search, CharPos(..), WordPos(..)) where

import qualified Data.Vector as V

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

listTo2DVec :: [[a]] -> V.Vector (V.Vector a)
listTo2DVec = V.fromList . map V.fromList

dirs8 :: [(Int, Int)]
dirs8 = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

findWord :: V.Vector (V.Vector Char) -> String -> Maybe WordPos
findWord grid word = tryStartAt squares
  where
    rMax = length grid
    cMax = if rMax == 0 then 0 else length (V.head grid)
    inGrid (c, r) = 1 <= r && r <= rMax && 1 <= c && c <= cMax
    wordLen = length word
    getEndSq (c, r) (dc, dr) = (c + (wordLen - 1) * dc, r + (wordLen - 1) * dr)
    squares = [(c, r) | r <- [1..rMax], c <- [1..cMax]]
    tryStartAt [] = Nothing
    tryStartAt (sq: sqs) = case try8Dirs dirs8 of
                             Just x -> Just x
                             Nothing -> tryStartAt sqs
      where
        try8Dirs [] = Nothing
        try8Dirs (dir@(dc', dr'): dirs) = if inGrid endSq && matchWord word sq
                               then Just (WordPos (uncurry CharPos sq) (uncurry CharPos endSq))
                               else try8Dirs dirs
          where
            endSq = getEndSq sq dir
            matchWord [] _ = True
            matchWord (w: ws) (c', r') = w == grid V.! (r' - 1) V.! (c' - 1) && matchWord ws (c' + dc', r' + dr')


search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = zip wordList $ map (findWord $ listTo2DVec grid) wordList