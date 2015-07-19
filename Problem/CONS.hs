{-
    Finding a Most Likely Common Ancestor

Problem

A matrix is a rectangular table of values divided into rows and columns. An m×n matrix
has m rows and n columns. Given a matrix A, we write Ai,j to indicate the value found
at the intersection of row i and column j.

Say that we have a collection of DNA strings, all having the same length n. Their profile
matrix is a 4×n matrix P in which P1,j represents the number of times that 'A' occurs in
the jth position of one of the strings, P2,j represents the number of times that C occurs
in the jth position, and so on (see below).

A consensus string c is a string of length n formed from our collection by taking the
most common symbol at each position; the jth symbol of c therefore corresponds to the
symbol having the maximum value in the j-th column of the profile matrix. Of course,
there may be more than one most common symbol, leading to multiple possible consensus strings.

DNA Strings 
    A T C C A G C T
    G G G C A A C T
    A T G G A T C T
    A A G C A A C C
    T T G G A A C T
    A T G C C A T T
    A T G G C A C T

Profile
A   5 1 0 0 5 5 0 0
C   0 0 1 4 2 0 6 1
G   1 1 6 3 0 1 0 0
T   1 5 0 0 0 1 1 6

Consensus
    A T G C A A C T

Given: A collection of at most 10 DNA strings of equal length (at most 1 kbp) in FASTA format.

Return: A consensus string and profile matrix for the collection. (If several possible
consensus strings exist, then you may return any one of them.)

Sample Dataset

>Rosalind_1
ATCCAGCT
>Rosalind_2
GGGCAACT
>Rosalind_3
ATGGATCT
>Rosalind_4
AAGCAACC
>Rosalind_5
TTGGAACT
>Rosalind_6
ATGCCATT
>Rosalind_7
ATGGCACT

Sample Output

ATGCAACT
A: 5 1 0 0 5 5 0 0
C: 0 0 1 4 2 0 6 1
G: 1 1 6 3 0 1 0 0
T: 1 5 0 0 0 1 1 6

-}

module Problem.CONS where

import Lib.Fasta

import Data.List (transpose)

testInput :: String
testInput = 
        ">Rosalind_1\n" ++
        "ATCCAGCT\n" ++
        ">Rosalind_2\n" ++
        "GGGCAACT\n" ++
        ">Rosalind_3\n" ++
        "ATGGATCT\n" ++
        ">Rosalind_4\n" ++
        "AAGCAACC\n" ++
        ">Rosalind_5\n" ++
        "TTGGAACT\n" ++
        ">Rosalind_6\n" ++
        "ATGCCATT\n" ++
        ">Rosalind_7\n" ++
        "ATGGCACT\n"

testOutput :: String
testOutput =
        "ATGCAACT\n" ++
        "A: 5 1 0 0 5 5 0 0\n" ++
        "C: 0 0 1 4 2 0 6 1\n" ++
        "G: 1 1 6 3 0 1 0 0\n" ++
        "T: 1 5 0 0 0 1 1 6\n"

test :: Bool
test = process testInput == testOutput

data ProfileMatrix = ProfileMatrix  { getA :: [Int]
                                    , getC :: [Int]
                                    , getG :: [Int]
                                    , getT :: [Int]
                                    }

process :: String -> String
process s = consensus ++ "\n" ++ (showProfileMatrix profileMatrix)
    where   fastaRecords = parseFastaToCharsRecords s
            consensus = calcConsensus profileMatrix
            profileMatrix = calcProfileMatrix fastaRecords

calcConsensus :: ProfileMatrix -> String
calcConsensus (ProfileMatrix [] _ _ _) = []
calcConsensus (ProfileMatrix (a:as) (c:cs) (g:gs) (t:ts)) = if and [a >= c, a >= g, a >= t] 
                                                            then 'A':other
                                                            else    if and [c >= g, c >= t]
                                                                    then 'C':other
                                                                    else    if g >= t 
                                                                            then 'G':other
                                                                            else 'T':other
    where   other = calcConsensus (ProfileMatrix (as) (cs) (gs) (ts))

calcProfileMatrix :: [FastaCharsRecord] -> ProfileMatrix
calcProfileMatrix rs = toPM $ transpose $ calcCountsPerColumn $ transpose fastaStrings
    where   toPM :: [[Int]] -> ProfileMatrix
            toPM [as, cs, gs, ts] = ProfileMatrix as cs gs ts
            fastaStrings :: [String]
            fastaStrings = map fastaStr rs
            calcCountsPerColumn :: [String] -> [[Int]]
            calcCountsPerColumn = map (foldl count [0,0,0,0])
            count :: [Int] -> Char -> [Int]
            count [a,c,g,t] (x)  = case x of
                                        'A' -> [a+1,c,g,t]
                                        'C' -> [a,c+1,g,t]
                                        'G' -> [a,c,g+1,t]
                                        'T' -> [a,c,g,t+1]

showProfileMatrix :: ProfileMatrix -> String
showProfileMatrix (ProfileMatrix a c g t) = unlines $
                                            ["A: " ++ (unwords $ map show a)
                                            ,"C: " ++ (unwords $ map show c)
                                            ,"G: " ++ (unwords $ map show g)
                                            ,"T: " ++ (unwords $ map show t)
                                            ]
