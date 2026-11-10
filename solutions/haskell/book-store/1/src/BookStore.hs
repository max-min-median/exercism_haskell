module BookStore (total, Book(..)) where

import Data.List (sort)
import qualified Data.Map as M

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Enum)

total :: [Book] -> Int
total basket = sum $ zipWith (*) prices normalized
  where
    sorted = reverse . sort . M.elems . M.fromListWith (+) $ [(book,1) | book <- basket] <> fmap (\x -> (x,0)) [First .. Fifth]
    sets = zipWith (-) sorted (tail sorted <> [0])
    normalized = case sets of
      [a,b,c,d,e] -> let toSwap = min c e in [a,b, c-toSwap, d+2*toSwap, e-toSwap]
      _           -> error "Unreachable"
    discounts = [0, 5, 10, 20, 25] 
    prices = zipWith (\d n -> 800 * n * (100-d) `div` 100) discounts [1..5]