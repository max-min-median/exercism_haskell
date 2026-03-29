module Bob (responseFor) where
import Data.Char ( isLower, isUpper, isSpace )
import Data.List ( dropWhile, dropWhileEnd )

responseFor :: String -> String
responseFor s
  | null str = "Fine. Be that way!"
  | allUpper && isQuestion = "Calm down, I know what I'm doing!"
  | allUpper = "Whoa, chill out!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where str = dropWhile isSpace . dropWhileEnd isSpace $ s
        allUpper = not (any isLower str) && any isUpper str
        isQuestion = last (dropWhileEnd isSpace str) == '?'
