module Frequency (frequency) where

import Data.Char (isAlpha)
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Parallel.Strategies

frequency :: Int -> [T.Text] -> M.Map Char Int
frequency nWorkers texts = M.unionsWith (+) (map toFreqMap texts `using` parListChunk n rdeepseq)
  where
    n = length texts `div` nWorkers
    toFreqMap text = T.foldl' (\mp ch -> M.insertWith (+) ch 1 mp) M.empty . T.toLower . T.filter isAlpha $ text
