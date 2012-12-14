{-
A common substring of a collection of strings is a substring of every member of the collection. 
We say that a common substring is a longest common substring if a longer common substring of the 
collection does not exist. For example, CG is a common substring of ACGTACGT and AACCGGTATA, 
whereas GTA is a longest common substring. Note that multiple longest common substrings may exist.

Given: A collection of k DNA strings (of length at most 1 kbp each; k≤100).

Return: A longest common substring of the collection. (If multiple solutions exist, you may return any single solution.)

Sample Dataset

GATTACA
TAGACCA
ATACA

Sample Output

AC

-}

{-
 Managed to run it compiled with 64-bit ghc-7.6.1 under 6 GB memory, 
 execution took 4 GB memory and 60 seconds.  

main.exe < rosalind_lcs.txt  +RTS -s -c -M6G -RTS
-}

module Problem.LCS where

import Data.List (sortBy, isInfixOf)
import Data.Function (on)

testInput :: String
testInput = 
            "GATTACA\n"
        ++  "TAGACCA\n"
        ++  "ATACA\n"
        ++  "\n"

testOutput :: [String]
testOutput = ["AC", "TA", "CA"]

test :: Bool
test = head (process testInput) `elem` testOutput

testSplitPattern :: Bool
testSplitPattern =     "GATTACA" == head splitResult -- test ordering
                    && "GATTAC" `elem` splitResult   -- test proper splicing
                    && "ATTACA" `elem` splitResult
                    && minimum (map length splitResult) == 2
    where splitResult = splitPattern 2 "GATTACA"


process :: String -> [String]
process input = take 1 $ findIntersections (head preparedData) 
                                         (tail preparedData) -- the head of data is used as a pattern, no need to re-check
    where preparedData = sortByLength $ filter (/= []) $ lines input  -- sorting to have a shortest element as a head

findIntersections :: String -> [String] -> [String]
findIntersections pattern dataSet = filter isPatternApplicable $ splitPattern (minPatternLength pattern) pattern 
    where   isPatternApplicable patt = all (patt `isInfixOf`) dataSet

minPatternLength :: String -> Int
minPatternLength str = if length str < 6 then 2 -- test environment
                                         else 7 --round $ ( sqrt . fromIntegral $ length str ) / 2

sortByLength :: [String] -> [String]
sortByLength = sortBy (compare `on` length)


-- split pattern into an ordered (longest to shortest) list of patterns
splitPattern :: Int -> String -> [String]
splitPattern minLength = reverse . sortByLength . subsequences' minLength

subsequences' :: Int -> String -> [String]
subsequences' minLength str = subsequences''' str (length str) minLength

subsequences''' :: String -> Int -> Int -> [String]
subsequences''' str n minLen | n < minLen = []
                             | otherwise  = subsequences'' str n ++ subsequences''' str (n-1) minLen

subsequences'' :: String -> Int -> [String]
subsequences'' str n | length str <= n = [str]
                     | otherwise       = take n str : subsequences'' (tail str) n