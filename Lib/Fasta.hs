module Lib.Fasta
    (parseFastaFileString, FastaFileString, FastaRecord(..))
    where

import Data.String.Utils ( strip, split )

import Lib.Nucleotide

type FastaFileString = String

data FastaRecord = FastaRecord 	{ desc :: String
                                , nucleoStr :: [Nucleotide]
                                }
                                deriving (Eq, Show, Read)

parseFastaFileString :: FastaFileString -> [FastaRecord]
parseFastaFileString s = map stringToRecord splitRecords
    where 	splitRecords = filter (/= []) $ split ">" $ strip s

stringToRecord :: String -> FastaRecord
stringToRecord s = FastaRecord { desc = head ls, nucleoStr = stringToNucleotides $ concat $ tail ls }
    where ls = lines s


