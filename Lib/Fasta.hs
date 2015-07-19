module Lib.Fasta (parseFastaToNucleoRecords, FastaFileString, FastaRecord(..), parseFastaToCharsRecords, FastaCharsRecord(..)) where

import Data.String.Utils ( strip, split )

import Lib.Nucleotide

type FastaFileString = String

data FastaRecord = FastaRecord  { desc :: String
                                , nucleoStr :: [Nucleotide]
                                }
                                deriving (Eq, Show, Read)

data FastaCharsRecord = FastaCharsRecord    { fastaDesc :: String
                                            , fastaStr :: [Char]
                                            }
                                            deriving (Eq, Show, Read)

parseFastaToNucleoRecords :: FastaFileString -> [FastaRecord]
parseFastaToNucleoRecords s = map strToNucleo $ parseFastaToCharsRecords s
    where   strToNucleo :: FastaCharsRecord -> FastaRecord
            strToNucleo (FastaCharsRecord id str) = FastaRecord id (stringToNucleotides $ str)

parseFastaToCharsRecords :: FastaFileString -> [FastaCharsRecord]
parseFastaToCharsRecords fileString = map stringToRecord splitRecords
    where   splitRecords = filter (/= []) $ split ">" $ unlines $ filter (/= []) $ map strip $ lines fileString
            stringToRecord :: String -> FastaCharsRecord
            stringToRecord ls = FastaCharsRecord (head $ lines ls) (concat $ tail $ lines ls)
