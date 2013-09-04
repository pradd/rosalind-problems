{-
Given: A protein string P of length at most 1000 aa.
Return: The total weight of P. Consult the monoisotopic mass table.

Sample Dataset
SKADYEK
Sample Output
821.392

-}

module Problem.PRTM where

import Lib.Amino (monoisotopic_mass)

testInput :: String
testInput = "SKADYEK"

testOutput :: Double
testOutput = 821.392

test :: Bool
test = process testInput == testOutput

process = round' . sum . map monoisotopic_mass 
    where round' x = fromIntegral (round (x * 1000)) / 1000
