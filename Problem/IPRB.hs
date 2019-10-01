{- 
Given: Three positive integers k, m, and n, representing a population containing k+m+n organisms:
k individuals are homozygous dominant for a factor, m are heterozygous, and n are homozygous recessive.

Return: The probability that two randomly selected mating organisms will produce an individual 
possessing a dominant allele (and thus displaying the dominant phenotype). Assume that any two organisms can mate.

Sample Dataset
2 2 2
Sample Output
0.78333
-}

module Problem.IPRB where


test :: Bool
test = take 7 (show (process' 2 2 2)) == "0.78333"

process' :: Int -> Int -> Int -> Double 
process' kk mm nn = process k m n
        where 
              k :: Double 
              k = fromIntegral kk
              m :: Double 
              m = fromIntegral mm
              n :: Double  
              n = fromIntegral mm 

process :: Double -> Double -> Double -> Double 
process k m n = (k*(k-1.0)  + 2.0 *m*k + 2.0 * k*n + (3.0/4.0)*m*(m-1.0) + m*n  ) / (k+m+n) / (k+m+n-1.0)

