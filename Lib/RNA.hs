module Lib.RNA where

import qualified Lib.Nucleotide as N
data RnaNucleotide = A | G | C | U deriving (Eq, Ord, Show, Read )

data RNA = RNA [RnaNucleotide] deriving (Ord, Eq)

instance Show RNA where
    show (RNA ns) = concat $ map show ns

fromString s = RNA (map (\x -> read [x]) s)

fromNucleotides :: [N.Nucleotide] -> RNA
fromNucleotides ns = RNA (map toRnaNucleo ns)
    where   toRnaNucleo N.T = U
            toRnaNucleo x   = read $ show x

toNucleotides :: RNA -> [N.Nucleotide]
toNucleotides (RNA ns) = map (read . show) ns
