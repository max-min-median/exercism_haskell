module FoodChain (song) where
import Data.Char (toLower)
import Data.List (intercalate)

data Animal = Fly | Spider | Bird | Cat | Dog | Goat | Cow | Horse
  deriving (Show, Eq, Enum, Bounded)

showLower :: Animal -> String
showLower = map toLower . show

reason :: Animal -> String
reason Fly = "I don't know why she swallowed the fly. Perhaps she'll die.\n"
reason Horse = ""
reason animal = "She swallowed the " ++ showLower animal ++ " to catch the " ++
  (case pred animal of
    Spider -> "spider that wriggled and jiggled and tickled inside her.\n"
    prev   -> showLower prev ++ ".\n"
  ) ++ reason (pred animal)

exclamation :: Animal -> String
exclamation animal = case animal of
  Fly    -> ""
  Spider -> "It wriggled and jiggled and tickled inside her.\n"
  Bird   -> "How absurd to swallow a bird!\n"
  Cat    -> "Imagine that, to swallow a cat!\n"
  Dog    -> "What a hog, to swallow a dog!\n"
  Goat   -> "Just opened her throat and swallowed a goat!\n"
  Cow    -> "I don't know how she swallowed a cow!\n"
  Horse  -> "She's dead, of course!\n"

stanza :: Animal -> String
stanza animal =
  "I know an old lady who swallowed a " ++ showLower animal ++ ".\n" ++
  exclamation animal ++
  reason animal

song :: String
song = intercalate "\n" . map stanza $ [minBound .. maxBound]