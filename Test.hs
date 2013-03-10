module Main where

import Problem.GC (test, testGcContent)
import Problem.PERM (test)
import Problem.LCSM (test, testSplitPattern)

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
        , (Problem.LCSM.test,               "LCSM  : main               ")
        , (Problem.LCSM.testSplitPattern,   "LCSM  : testSplitPattern   ")
        ]
