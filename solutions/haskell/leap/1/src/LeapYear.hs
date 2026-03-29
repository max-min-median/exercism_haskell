module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
    | mod year 400 == 0 = True
    | mod year 100 == 0 = False
    | otherwise = mod year 4 == 0
