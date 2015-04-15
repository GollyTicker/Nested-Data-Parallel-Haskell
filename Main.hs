
module Main (main) where

-- The Main module which uses DotP.hs to execute parallel array dot product.

-- Compile using:   ./build.sh
-- Run with:        ./run.sh

-- Example made using tutorials from 
--  Data Parallel Haskell: A Tutorial for the Curious Haskeller: http://www.cse.chalmers.se/edu/course/DAT280_Parallel_Functional_Programming/Papers/DPHTutorial13.pdf
--  HaskellWiki - Data Parallel Haskell: https://wiki.haskell.org/GHC/Data_Parallel_Haskell#Running_DPH_programs

import Data.Array.Parallel
import Data.Array.Parallel.PArray(PArray, fromList)

import DotP (dotpWrapper)

main :: IO ()
main =
    let v   = fromList [1..10]
        w   = fromList [0,2..20]
        result  = dotpWrapper v w
    in  print result

