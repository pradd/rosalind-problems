module Lib.RNA where

import qualified Lib.Nucleotide as N

data RnaNucleotide = A | G | C | U deriving (Eq, Ord, Show, Read )

data RNA = RNA [RnaNucleotide] deriving (Ord, Eq)

instance Show RNA where
    show (RNA ns) = concatMap show ns

fromString :: String -> RNA
fromString s = RNA (map (\x -> read [x]) s)

nucleotidesToRna :: [N.Nucleotide] -> RNA
nucleotidesToRna ns = RNA (map toRnaNucleo ns)
    where   toRnaNucleo N.T = U
            toRnaNucleo x   = read $ show x

rnaToNucleotides :: RNA -> [N.Nucleotide]
rnaToNucleotides (RNA ns) = map (read . show) ns
