
module Main (main) where

-- The Main module which uses DotP.hs to execute parallel array dot product.

-- Compile using:  . ./build.sh
-- Run with:        ./run.sh

import Data.Array.Parallel
import Data.Array.Parallel.PArray(PArray, fromList)

import DotP (dotp_wrapper)

main :: IO ()
main =
    let v   = fromList [1..10]
        w   = fromList [0,2..20]
        result  = dotp_wrapper v w
    in  print result

