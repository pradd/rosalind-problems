{-
Given: Positive integers n ≤40 and k ≤5.

Return: The total number of rabbit pairs that will be present after n months if we begin with 1 pair and in each generation, 
every pair of reproduction-age rabbits produces a litter of k rabbit pairs (instead of only 1 pair).

Sample Dataset
5 3
Sample Output
19

-}

module Problem.FIB where

test :: Bool
test = (process 5 3) == 19


process :: Integer -> Integer -> Integer
process n k = let (young, old) = rabbits n k  
              in  young + old

rabbits 1 _ = (1, 0)
rabbits 2 _ = (0, 1)
rabbits n k = (old * k, old + young) 
    where (young, old) = rabbits (n - 1) k


