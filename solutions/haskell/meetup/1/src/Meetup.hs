module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
  deriving Enum

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
  First  -> firstDay
  Second -> addDays 7 firstDay
  Third  -> addDays 14 firstDay
  Fourth -> addDays 21 firstDay
  Last   -> addDays ((fromIntegral daysInMonth - 1 - offset) `div` 7 * 7) firstDay
  Teenth -> addDays ((19 - 1 - offset) `div` 7 * 7) firstDay
  where
    day1 = YearMonthDay year month 1
    dayOfWeek1 = fromEnum $ dayOfWeek day1
    offset = fromIntegral (requiredDOW - dayOfWeek1) `mod` 7
    firstDay = addDays offset day1
    requiredDOW = fromEnum weekday + 1
    daysInMonth = gregorianMonthLength year month
