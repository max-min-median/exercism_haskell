module Knapsack (maximumValue) where

import Control.Monad.Trans.State ( evalState, gets, State )
import qualified Data.Map as M

type Weight = Int
type Value = Int
type Item = (Weight, Value)

knapsack :: Weight -> [Item] -> State (M.Map (Weight, [Item]) (Value, [Item])) (Value, [Item])
knapsack _ [] = pure (0, [])
knapsack weight items
  | weight == 0 = pure (0, [])
  | null items  = pure (0, drop 1 items)
knapsack weight items@(item@(wt, val): restItems) = do
  cached <- gets $ M.lookup (weight, items)
  case cached of
    Just result -> pure result
    Nothing     -> do
      leaveThisItem <- knapsack weight restItems
      if weight < wt then pure leaveThisItem else do
        (valIfTake, itemsIfTake) <- knapsack (weight-wt) restItems
        pure $ max (val+valIfTake, item:itemsIfTake) leaveThisItem

maximumValue :: Weight -> [(Weight, Value)] -> Value
maximumValue weight items = fst $ evalState (knapsack weight items) M.empty
