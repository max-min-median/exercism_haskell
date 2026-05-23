module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck
import Control.Monad

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier x = (x - 10) `div` 2

ability :: Gen Int
ability = do
  a <- vectorOf 4 (choose (1, 6))
  return $ sum a - minimum a

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  pure $ Character str dex con int wis cha hp