module ProteinTranslation (proteins) where

import Data.List.Split
import qualified Data.Map as M

data Protein = Protein String | STOP

codonMap :: M.Map String (Maybe String)
codonMap = M.fromList [
                      ("AUG", Just "Methionine"),
                      ("UUU", Just "Phenylalanine"),
                      ("UUC", Just "Phenylalanine"),
                      ("UUA", Just "Leucine"),
                      ("UUG", Just "Leucine"),
                      ("UCU", Just "Serine"),
                      ("UCC", Just "Serine"),
                      ("UCA", Just "Serine"),
                      ("UCG", Just "Serine"),
                      ("UAU", Just "Tyrosine"),
                      ("UAC", Just "Tyrosine"),
                      ("UGU", Just "Cysteine"),
                      ("UGC", Just "Cysteine"),
                      ("UGG", Just "Tryptophan"),
                      ("UAA", Nothing),
                      ("UAG", Nothing),
                      ("UGA", Nothing)
                    ]

proteins :: String -> Maybe [String]
proteins = foldr step (Just []) . map (`M.lookup` codonMap) . chunksOf 3
  where
    step (Just Nothing) _ = Just []
    step Nothing _ = Nothing
    step _ Nothing = Nothing
    step (Just (Just x)) (Just xs) = Just (x:xs)
