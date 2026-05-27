module ProteinTranslation (proteins) where

import Data.List.Split
import qualified Data.Map as M

data Protein = Protein String | STOP


proteinToString :: Protein -> String
proteinToString STOP = "<STOP>"
proteinToString (Protein x) = x


codonMap :: M.Map String Protein
codonMap = M.fromList [
                      ("AUG", Protein "Methionine"),
                      ("UUU", Protein "Phenylalanine"),
                      ("UUC", Protein "Phenylalanine"),
                      ("UUA", Protein "Leucine"),
                      ("UUG", Protein "Leucine"),
                      ("UCU", Protein "Serine"),
                      ("UCC", Protein "Serine"),
                      ("UCA", Protein "Serine"),
                      ("UCG", Protein "Serine"),
                      ("UAU", Protein "Tyrosine"),
                      ("UAC", Protein "Tyrosine"),
                      ("UGU", Protein "Cysteine"),
                      ("UGC", Protein "Cysteine"),
                      ("UGG", Protein "Tryptophan"),
                      ("UAA", STOP),
                      ("UAG", STOP),
                      ("UGA", STOP)
                    ]

proteins :: String -> Maybe [String]
proteins = Just . map proteinToString . foldr step [] . map (`M.lookup` codonMap) . chunksOf 3
  where
    step (Just STOP) _ = []
    step Nothing _ = []
    step (Just x) xs = x:xs
