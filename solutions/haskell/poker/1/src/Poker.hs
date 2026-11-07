module Poker (bestHands) where

import Data.List (group, groupBy, sortOn, nub)
import Data.Ord  (Down(Down))
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)
import Data.Function (on)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Show)
data Card = Card {cardRank :: Int, cardSuit :: Suit} deriving (Eq, Ord, Show)

data PokerHand = HighCard Int Int Int Int Int
               | Pair Int Int Int Int
               | TwoPair Int Int Int
               | ThreeOfAKind Int Int Int
               | Straight Int
               | Flush Int Int Int Int Int
               | FullHouse Int Int
               | FourOfAKind Int Int
               | StraightFlush Int
  deriving (Eq, Ord, Show)

bestHands :: [String] -> Maybe [String]
bestHands xs = do
  ranks <- traverse (fmap getRank . parseCards) xs
  let rankedHands = groupBy ((==) `on` fst) . sortOn Down . zip ranks $ xs
  map snd <$> listToMaybe rankedHands

getRank :: [Card] -> PokerHand
getRank xs
  | isFlush && isStraight = StraightFlush straightHigh
  | counts == [4,1] = FourOfAKind a e
  | counts == [1,4] = FourOfAKind b a
  | counts == [3,2] = FullHouse a d
  | counts == [2,3] = FullHouse c a
  | isFlush             = Flush a b c d e
  | isStraight          = Straight straightHigh
  | counts == [3,1,1] = ThreeOfAKind a d e
  | counts == [1,3,1] = ThreeOfAKind b a e
  | counts == [1,1,3] = ThreeOfAKind c a b
  | counts == [1,2,2]   = TwoPair b d a
  | counts == [2,1,2]   = TwoPair a d c
  | counts == [2,2,1]   = TwoPair a c e
  | counts == [2,1,1,1] = Pair a c d e
  | counts == [1,2,1,1] = Pair b a d e
  | counts == [1,1,2,1] = Pair c a b e
  | counts == [1,1,1,2] = Pair d a b c
  | otherwise           = HighCard a b c d e
  where
    ranks = map cardRank xs
    (a,b,c,d,e) = case ranks of
      [p,q,r,s,t] -> (p,q,r,s,t)
      _           -> error "Unreachable"
    isStraight = if (b-c,c-d,d-e) /= (1,1,1) then False
                 else a-b == 1 || (a,b) == (14,5)  -- [A,5,4,3,2]
    straightHigh = if a-b == 1 then a else b
    isFlush = length (nub (map cardSuit xs)) == 1
    counts = map length (group ranks)


-- | Parses the string representing the hand, ensuring that it comprises exactly 5 valid, distinct cards.
--   Also sorts the hand in descending order.
parseCards :: String -> Maybe [Card]
parseCards xs = case sortOn Down <$> traverse parseCard (words xs) of
  Just hand@[_,_,_,_,_] -> if length (nub hand) == 5 then Just hand else Nothing
  _                     -> Nothing

parseCard :: String -> Maybe Card
parseCard card = do
  (rankStr, suitStr) <- unsnoc card
  suit <- case suitStr of 'C' -> Just Clubs
                          'D' -> Just Diamonds
                          'H' -> Just Hearts
                          'S' -> Just Spades
                          _   -> Nothing
  rank <- case rankStr of "J" -> Just 11
                          "Q" -> Just 12
                          "K" -> Just 13
                          "A" -> Just 14
                          _   -> (readMaybe rankStr :: Maybe Int) >>= \r -> if r < 2 || r > 10 then Nothing else pure r
  pure $ Card rank suit

-- no 'unsnoc' in exercism...
unsnoc :: [a] -> Maybe ([a], a)
unsnoc []  = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = (\(a,b) -> (x:a,b)) <$> unsnoc xs