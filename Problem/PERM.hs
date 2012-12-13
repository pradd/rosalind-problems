{-
Problem

A permutation of length n is some ordering of the positive integers {1,2,…,n}. For example, π=(5,3,2,1,4) is a permutation of length 5.

Given: A positive integer n≤7.

Return: The total number of permutations of length n, followed by a list of all such permutations (in any order).

Sample Dataset

3
Sample Output

6
1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1
-}

module Problem.PERM where

import Data.List (sort)

testInput :: Int
testInput = 3

testOutput :: String
testOutput = "6\n"
        ++ "1 2 3\n"
        ++ "1 3 2\n"
        ++ "2 1 3\n"
        ++ "2 3 1\n"
        ++ "3 1 2\n"
        ++ "3 2 1\n"

test :: Bool
test = process testInput == testOutput

process :: Int -> String
process n = toPrintableFormat (length perms) perms
    where perms = permutations n

toPrintableFormat :: Int -> [[Int]] -> String
toPrintableFormat n xs = unlines (show n : matrixToPrintableFormat (sort xs))

matrixToPrintableFormat :: [[Int]] -> [String]
matrixToPrintableFormat = map lineToString 
    where   lineToString xs = unwords $ map show xs

permutations :: Int -> [[Int]]
permutations 1 = [[1]]  
-- intersperse each previous (n-1) permutation with n
-- is nub required ?
permutations 2 = [[2,1], [1,2]] 
permutations n = concatMap (intersperse' [] n) $ permutations (n-1)

intersperse' :: [Int] -> Int -> [Int] -> [[Int]]
intersperse' hs n []    = [hs ++ [n]]
intersperse' hs n ts    = (hs ++ [n] ++ ts) : intersperse' (hs ++ [head ts]) n (tail ts)








