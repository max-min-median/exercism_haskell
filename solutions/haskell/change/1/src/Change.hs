module Change (findFewestCoins) where

import qualified Data.Map as M
import Data.List (sort, uncons, foldl')
import Control.Monad (forM)
import Control.Monad.Trans.State (evalState, gets, modify', State)

type Target = Integer
type Coin = Integer
type Count = Integer

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target unsortedCoins = fmap snd $ evalState (coinsMemo target coins) (M.fromList [((0, []), Just (0, []))])
  where
    coins = reverse . sort $ unsortedCoins

    coinsMemo :: Target -> [Coin] -> State (M.Map (Target, [Coin]) (Maybe (Count, [Coin]))) (Maybe (Count, [Coin]))
    coinsMemo tgt coinList = do
      memoLookup <- gets (M.lookup (tgt, coinList))
      case memoLookup of
        Just cached -> pure cached
        _           -> case uncons coinList of
          Nothing           -> pure Nothing
          Just (coin, rest) -> do
            results <- forM (zip [tgt, tgt-coin .. 0] [0..]) $ \(newTgt, k) -> do
              result <- coinsMemo newTgt rest
              pure $ do
                (newCount, newResultList) <- result
                pure (newCount+k, replicate (fromIntegral k) coin ++ newResultList)
            let best = foldl' getMin Nothing results
            modify' (M.insert (tgt, coinList) best)
            pure best

    getMin :: Maybe (Integer, [Integer]) -> Maybe (Integer, [Integer]) -> Maybe (Integer, [Integer])
    getMin Nothing x = x
    getMin x Nothing = x
    getMin x y = min x y