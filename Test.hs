module Main where

import Lib.Fasta
import Problem.GC (test, testGcContent)
import Problem.PERM (test)
import Problem.LCSM (test, testSplitPattern)
import Problem.SIGN (test)

main :: IO ()
main = putStrLn testResults 

testResults :: String
testResults = unlines $ map out tests   
    where out (result, desc) | result    = desc ++ "  OK"
                             | otherwise = desc ++ "  FAILED"

tests :: [(Bool, String)]
tests = [ (Problem.GC.test,                 "GC   : main                ")
        , (Problem.GC.testGcContent,        "GC   : gc-content          ")
        , (Problem.PERM.test,               "PERM : main                ")
        , (Problem.LCSM.test,               "LCSM : main                ")
        , (Problem.LCSM.testSplitPattern,   "LCSM : testSplitPattern    ")
        , (Problem.SIGN.test,               "SIGN : test                ")
        , (fastaStringParserTest,           "Lib.Fasta : string parser  ")
        ]

fastaStringParserTest :: Bool
fastaStringParserTest = parseFastaToCharsRecords input == expected
    where   input = ">Rosalind_1\n" ++
                    "AAAAAAA\n" ++
                    ">Rosalind_2 \n" ++
                    " GGGGGGGG \n" ++
                    " TTTTTTTT \n"
            expected = [ FastaCharsRecord "Rosalind_1" "AAAAAAA"
                       , FastaCharsRecord "Rosalind_2" "GGGGGGGGTTTTTTTT"
                       ]
