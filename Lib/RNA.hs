module Lib.RNA where

import Lib.Nucleotide

data RNA = RNA [Nucleotide] deriving (Ord, Eq)

instance Show RNA where
    show (RNA ns) = concatMap show ns

fromString :: String -> RNA
fromString s = nucleotidesToRnaValidating $ stringToNucleotides s

nucleotidesToRna :: [Nucleotide] -> RNA
nucleotidesToRna ns = RNA (map toRnaNucleo ns)
    where   toRnaNucleo T = U
            toRnaNucleo x = x

nucleotidesToRnaValidating :: [Nucleotide] -> RNA
nucleotidesToRnaValidating ns | T `elem` ns = error "Nucleotide T is not allowed for RNA"
                              | otherwise   = nucleotidesToRna ns

rnaToNucleotides :: RNA -> [Nucleotide]
rnaToNucleotides (RNA ns) = ns
