module Main where

import Problem.GC (test)

main = print $ if test then "GC ok" else "GC failed"
