module Lib.DNA where

import Lib.Nucleotide

data DNA = DNA [Nucleotide] deriving (Ord, Eq)

instance Show DNA where
    show (DNA ns) = concatMap show ns

fromString :: String -> DNA
fromString s = nucleotidesToDnaValidating $ stringToNucleotides s

nucleotidesToDna :: [Nucleotide] -> DNA
nucleotidesToDna ns = DNA (map toDnaNucleo ns)
    where   toDnaNucleo U = T
            toDnaNucleo x = x

nucleotidesToDnaValidating :: [Nucleotide] -> DNA
nucleotidesToDnaValidating ns | U `elem` ns = error "Nucleotide U is not allowed for DNA"
                              | otherwise   = nucleotidesToDna ns

dnaToNucleotides :: DNA -> [Nucleotide]
dnaToNucleotides (DNA ns) = map (read . show) ns
