module ResistorColors (Color(..), Resistor(..), label, ohms) where

import Control.Applicative (asum)
import Data.Maybe (fromMaybe)

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor = fromMaybe (show value ++ " ohms") $ asum (map trySuffix suffixes)
  where
    value = ohms resistor
    suffixes = [(1000000000, "gigaohms"),
                (1000000, "megaohms"),
                (1000, "kiloohms")
               ]
    trySuffix (expo, sfx) = if value >= expo then Just (show (value `div` expo) ++ " " ++ sfx) else Nothing

ohms :: Resistor -> Int
ohms (Resistor (a, b, c)) = (fromEnum a * 10 + fromEnum b) * 10 ^ fromEnum c