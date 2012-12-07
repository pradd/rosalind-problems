module Lib.Nucleotide 
	(stringToNucleotides, Nucleotide(..))
	where

data Nucleotide = A | G | C | U | T deriving (Eq, Ord, Show, Read )

stringToNucleotides :: String -> [Nucleotide]
stringToNucleotides = map charToNucleotide
	where charToNucleotide ch = read [ch]