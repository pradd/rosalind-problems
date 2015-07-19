{-
Problem

A signed permutation of length n is some ordering of the positive integers {1,2,…,n}
in which each integer is then provided with either a positive or negative sign 
(for the sake of simplicity, we omit the positive sign). For example, π=(5,−3,−2,1,4)
is a signed permutation of length 5.

Given: A positive integer n≤6.

Return: The total number of signed permutations of length n, followed by a list of all 
such permutations (you may list the signed permutations in any order).

Sample Dataset

2

Sample Output

8
-1 -2
-1 2
1 -2
1 2
-2 -1
-2 1
2 -1
2 1

-}

module Problem.SIGN where

import qualified Data.Set as S

testInput :: Int
testInput = 2

testOutput :: (Int, S.Set [Int])
testOutput = (8, S.fromList [
    [-1,  -2],
    [-1,  2],
    [1 , -2],
    [1 , 2],
    [-2,  -1],
    [-2,  1],
    [2 , -1],
    [2 , 1]
    ])

test :: Bool
test = (calc testInput) == testOutput

calc :: Int -> (Int, S.Set [Int])
calc n = (length perms, S.fromList perms)
    where   perms   = concatMap applySigns $ permutations n

applySigns :: [Int] -> [[Int]]
applySigns  [x]    = [[-x], [x]] 
applySigns  (x:xs) =  (map (x:) updatedTail ) ++ (map ((-x) :) updatedTail )
    where   updatedTail :: [[Int]]
            updatedTail = applySigns xs

permutations :: Int -> [[Int]]
permutations 1 = [[1]]  
-- intersperse each previous (n-1) permutation with n
-- is nub required ?
permutations 2 = [[2,1], [1,2]] 
permutations n = concatMap (intersperse' [] n) $ permutations (n-1)

intersperse' :: [Int] -> Int -> [Int] -> [[Int]]
intersperse' hs n []    = [hs ++ [n]]
intersperse' hs n ts    = (hs ++ [n] ++ ts) : intersperse' (hs ++ [head ts]) n (tail ts)

process :: String -> String
process = format . calc . read

format :: (Int, S.Set [Int]) -> String
format (n, s) = unlines $ [show n] ++ (map showList $ S.elems s)
    where   showList l = unwords $ map show l
