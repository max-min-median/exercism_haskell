module House (rhyme) where
import Data.List (intercalate)

thingsAndActions :: [(String, String)]
thingsAndActions = [
                     ("horse and the hound and the horn", "belonged to"),
                     ("farmer sowing his corn", "kept"),
                     ("rooster that crowed in the morn", "woke"),
                     ("priest all shaven and shorn", "married"),
                     ("man all tattered and torn", "kissed"),
                     ("maiden all forlorn", "milked"),
                     ("cow with the crumpled horn", "tossed"),
                     ("dog", "worried"),
                     ("cat", "killed"),
                     ("rat", "ate"),
                     ("malt", "lay in")
                   ]

stanza :: Int -> String
stanza n = "This is" ++ middle ++ " the house that Jack built.\n"
  where
    lastN = reverse . take n . reverse $ thingsAndActions
    middle = concatMap (\(noun, verb) -> " the " ++ noun ++ "\n" ++ "that " ++ verb ) $ lastN

rhyme :: String
rhyme = intercalate "\n" . take 12 . map stanza $ [0..]