{-
Given two strings s=s1s2?sn and t=t1t2?tm where m?n, t is a substring of s if t is contained as a contiguous collection of symbols in s.

The position of a symbol in a string is the total number of symbols found to its left, 
including itself (e.g., the positions of all occurrences of U in 
AUGCUUCAGAAAGGUCUUACG are 2, 5, 6, 15, 17, and 18). A location of t is a position in s 
at which t begins; note that t will have multiple locations in s if it occurs more than 
once as a substring of s (see the Sample sections below).

Given: Two DNA strings s and t (each of length at most 1 kbp).

Return: All locations of t as a substring of s.

Sample Dataset
ACGTACGTACGTACGT
GTA
Sample Output
3 7 11
-}

import Data.List

input = "TTTCTGAGTTCTGAGTTCGCCCTCTGAGTTCTGAGTTCTGAGTTTTTTGCTCTCTGAGTAAGCTCTGAGTGGATTCTGAGTTCTGAGTTCTGAGTTCTGAGTGCGTTCTGAGTCTCTGAGTGTCTGAGTACTCTGAGTTCTGAGTCGTCTGAGTAGCAGTCTGAGTTCTGAGTTCTGAGTGTCTGAGTTTCGACTAGTCTGAGTTCTGAGTTCTGAGTGTCTGAGTCGTCTGAGTTCTGAGTTCTGAGTCTCTGAGTTCTGAGTAAGATCTGAGTTCTGAGTTCCGTAGACTCTGAGTATCTGAGTAGGTCTGAGTCACGGGTATATTGTCTGAGTAGCTCTGAGTAGAAGATCTGAGTACAATCTGAGTGTCTGAGTTTCTGAGTGGGGATCTGAGTCGAATCTGAGTGTCTGAGTCATCTGAGTCCTCTGAGTGATAGACTGTCTGAGTCTCTGAGTGTCTGAGTCGCTCTGAGTTGACTCCCCAGAGATCTGAGTTCTGAGTTTTGCATCTGAGTCAGATCTGAGTCTCTGAGTACAATTTGATCTGAGTCTCTGAGTTAGGATCTGAGTGATCTCTGAGTGTATCTGAGTTTTCTGAGTTTCTGAGTCTGTACGTCTGAGTTCTGAGTCTCTGAGTTCCGCCCGATCTGAGTGTCTGAGTGTTCTGAGTAGTTCTGAGTTAACTCTGAGTTCTGAGTTCTGAGTTTCATCTGAGTGTCTGAGTAGTCTGAGTTCTGAGTACTCTGAGTTCTGAGTGACTTCTGAGTGGTCTGAGTAATCTGAGTATCTGAGTTCTGAGTCTCTGAGTTGTCTGAGTTCTGAGTTCTGAGTTCTGAGT"
substring = "TCTGAGTTC"

positions _ [] _ = []
positions pos string@(x:xs) substring | isPrefixOf substring string = pos : (positions  (pos+1) xs substring)
                                      | otherwise                   =        positions  (pos+1) xs substring

main = putStrLn $ unwords $ map show $ positions 1 input substring
