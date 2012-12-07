module Lib.DNA where

import qualified Lib.Nucleotide as N

data DnaNucleotide = A | G | C | T deriving (Eq, Ord, Show, Read )

data DNA = DNA [DnaNucleotide] deriving (Ord, Eq)

instance Show DNA where
    show (DNA ns) = concat $ map show ns

fromString s = DNA (map (\x -> read [x]) s)

nucleotidesToDna :: [N.Nucleotide] -> DNA
nucleotidesToDna ns = DNA (map toDnaNucleo ns)
    where   toDnaNucleo N.U = T
            toDnaNucleo x   = read $ show x

dnaToNucleotides :: DNA -> [N.Nucleotide]
dnaToNucleotides (DNA ns) = map (read . show) ns
