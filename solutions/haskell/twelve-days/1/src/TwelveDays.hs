module TwelveDays (recite) where

import Data.List (intercalate)

dayStr :: Int -> String
dayStr = (!!) ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"] . (subtract 1)

gifts :: [String]
gifts = ["twelve Drummers Drumming", "eleven Pipers Piping", "ten Lords-a-Leaping", "nine Ladies Dancing", "eight Maids-a-Milking",
         "seven Swans-a-Swimming", "six Geese-a-Laying", "five Gold Rings", "four Calling Birds", "three French Hens",
         "two Turtle Doves", "a Partridge in a Pear Tree"]

reciteDay :: Int -> String
reciteDay day = "On the " ++ dayStr day ++ " day of Christmas my true love gave to me: " ++ (intercalate ", " . init . drop (12-day) $ gifts)
                ++ (if day > 1 then ", and " else "") ++ last gifts ++ "."
                                       

recite :: Int -> Int -> [String]
recite start stop = map reciteDay [start..stop]
