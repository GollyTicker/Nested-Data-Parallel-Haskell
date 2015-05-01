{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

-- The fully vectorized Module using Parallel Arrays

module Algo (
       -- oneStep
        minBySndP
    ) where
 
import qualified Prelude as P
import Data.Array.Parallel hiding ((+),(-),(<),div)
import Data.Array.Parallel.Prelude hiding ((+),(-),(<),div)
import Data.Array.Parallel.Prelude.Int

type Vector = [:Double:]
type SparseVector = [:(Double,Int):]
type SparseMatrix = [:SparseVector:]
-- Dijkstra

type Pred = Int
l, x, r :: Pred
l = 0 - 1
x = 0
r = 1
preds = [: l,x,r:]

data Preds = Nil | Cons Pred Preds

predAdd p ps = Cons p ps

-- its assumed that slice i j [: 0,1,..,i,...,j,...z :] = [: i, ... j :]
-- that means, that slice takes the all elements inbetween i and j including them.

oneStep :: [: (Preds,Int) :] -> [: Int :] -> [: (Preds,Int) :]
oneStep psis row = res
    where
        pss :: [: [: (Preds,Int) :] :]
        pss = [: ps'
                | i <- preds,
                let ps = sliceP i (i + l - 1) psis,
                let ps' = zipWithP (f i) ps row :]
                
        f :: Int -> (Preds,Int) -> Int -> (Preds,Int)
        f i (ps,s) n = (predAdd i ps, s + n)
                
        indices :: [: Int :]
        indices = enumFromToP 0 (lengthP row - 1)
        
        res :: [: (Preds,Int) :]
        res = [: minBySndP ps
                | i <- indices,
                let ps = mapP (!: i) pss :]

minBySndP :: [: (Preds,Int) :] -> (Preds,Int) 
-- minByP :: (Ord b) => (a -> b) -> [: a :] -> a
-- using monomorphic variant, since type-classes are not yet supported in dph

minBySndP ps = if lengthP ps < 2 then ps !: 0 else result
    where
        f :: (Preds,Int) -> Int
        f = \(a,b) -> b
        pivot = lengthP ps `div` 2
        psL  = sliceP 0 pivot ps
        psR = sliceP (pivot + 1) (lengthP ps - 1) ps
        minBoth = [: minBySndP ps' | ps' <- [: psL,psR :] :]
        minL = minBoth !: 0
        minR = minBoth !: 1
        result = if f minL < f minR
                    then minL
                    else minR

