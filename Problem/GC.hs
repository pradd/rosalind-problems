{-
The GC-content of a DNA string is given by the percentage of symbols 
in the string that are 'C' or 'G'. For example, the GC-content of "AGCTATAG" is 37.5%.
Note that the reverse complement of any DNA string has the same GC-content.

DNA strings must be labeled when they are consolidated into a database. 
A commonly used method of string labeling is called FASTA format. 
In this format, the string is introduced by a line that begins with '>', 
followed by some labeling information. Subsequent lines contain the string itself; 
the first line to begin with '>' indicates the label of the next string.

In Rosalind's implementation, a string in FASTA format will be labeled by the ID "Rosalind_xxxx", 
where "xxxx" denotes a four-digit code between 0000 and 9999.

Given: At most 10 DNA strings in FASTA format (of length at most 1 kbp each).

Return: The ID of the string having the highest GC-content, followed by the GC-content of that string. 
The GC-content should have an accuracy of 3 decimal places (please refer to the note below on decimal accuracy).

Sample Dataset

>Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT

Sample Output

Rosalind_0808
60.919540%
-}

module Problem.GC where

import Text.Printf (printf)

import Lib.Nucleotide
import Lib.DNA
import Lib.Fasta

testInput :: FastaFileString
testInput =    ">Rosalind_6404\n"
            ++ "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC\n"
            ++ "TCCCACTAATAATTCTGAGG\n"
            ++ ">Rosalind_5959\n"
            ++ "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT\n"
            ++ "ATATCCATTTGTCAGCAGACACGC\n"
            ++ ">Rosalind_0808\n"
            ++ "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC\n"
            ++ "TGGGAACCTGCGGGCAGTAGGTGGAAT\n"

testOutput :: String
testOutput =    "Rosalind_0808\n"
            ++  "60.919540%\n"

gcContent :: DNA -> Float
gcContent (DNA dna) = fromIntegral gcCount / fromIntegral total
        where gcCount = length $ filter isGC dna
              total = length dna
              isGC G = True
              isGC C = True
              isGC _ = False

process :: FastaFileString -> String 
process input = toPrintableFormat $ foldl1 max' $ map countGcContent $ parseFastaFileString input 
    where countGcContent (FastaRecord { desc = d, nucleoStr = ns}) = (d, gcContent $ nucleotidesToDna ns)
          max' x@(_, gc1) y@(_, gc2) | gc2 > gc1 = y
                                     | otherwise = x
          toPrintableFormat (d, number) = unlines [d, printPercent number]

printPercent :: Float -> String
printPercent float = printf "%.6f" (float * 100) ++ "%"

test :: Bool
test = process testInput == testOutput

testGcContent :: Bool
testGcContent = gcContent (fromString "AGCTATAG") == 0.375 


