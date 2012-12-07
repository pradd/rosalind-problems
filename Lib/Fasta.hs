module Lib.Fasta
    (parseFastaFileString, FastaFileString, FastaData(..), FastaRecord(..))
    where

import Data.String.Utils ( strip, split )

import Lib.Nucleotide

type FastaFileString = String

data FastaData = FastaData [FastaRecord] deriving (Eq, Show, Read)

data FastaRecord = FastaRecord 	{ desc :: String
                                , nucleoStr :: [Nucleotide]
                                }
                                deriving (Eq, Show, Read)

splitNonRemoving :: String -> String -> [String]
splitNonRemoving ch str = map (ch ++) $ filter (/= "") $ split ch str

parseFastaFileString :: FastaFileString -> FastaData
parseFastaFileString s = FastaData (map stringToRecord splitRecords)
    where 	splitRecords = splitNonRemoving ">" $ strip s

stringToRecord :: String -> FastaRecord
stringToRecord s = FastaRecord { desc = head ls, nucleoStr = stringToNucleotides $ concat $ tail ls }
    where ls = lines s


