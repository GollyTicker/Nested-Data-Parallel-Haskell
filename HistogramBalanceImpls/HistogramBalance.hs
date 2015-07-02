{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

-- The data parallel variant using Nested Data Parallelism in Haskell

module HistogramBalance (
        hbalance,
        hbalanceBulk
    ) where
 
import qualified Prelude as P
import Data.Array.Parallel hiding ((+),(-),(<),div)
import Data.Array.Parallel.Prelude hiding ((+),(-),(<),div)
import Data.Array.Parallel.Prelude.Int
import qualified Data.Array.Parallel.Prelude.Double as D

import Utils
{-
Es gibt die freie Entscheidung hierbei, ob das
Histogram als Map [:(Int,a):] oder als dense Array [: a :]
realisiert werden soll. 
-}

type Many a  = [: a :]
type Image a = [:[: a :]:]

type Hist a = [: a :]

(.) = (P..)

hbalanceBulk :: Many (Image Int) -> Many (Image Int)
hbalanceBulk = mapP hbalance

hbalance :: Image Int -> Image Int
hbalance img =
    let h = hist img
        a = accu h
        a0 = headP a
        agmax = lastP a
        n = normalize a0 agmax a
        s = scale gmax n
        img' = apply s img
    in  img'

gmax :: Int
gmax = 255      -- der maximale Grauwert

hist :: Image Int -> Hist Int
hist = 
    sparseToDenseP (gmax+1) 0
    . mapP (\g -> (headP g,lengthP g))
    . groupP
    . sortP
    . concatP

{-

sparseToDenseP :: Enum e => e -> a -> [: (e,a) :] -> [: a :]
sparseToDenseP              many init map            result
sparseToDenseP 8 0 [: (1,5),(2,4),(6,7) :] == [: 0,5,4,0,0,0,7,0 :]

sparseToDense n z map creates an array of length n where the element
at the index i has x if (i,x) is in the map, or z otherwise.
In effect it turns a sparse vector to a dense one

O(many)
Depth: 1
Work: many + lengthP map

-}

accu :: Hist Int -> Hist Int
accu = scanlP (+) 0

normalize :: Int {-a0-} -> Int {-a(gmax)-} -> Hist Int -> Hist Double
normalize a0' agmax' as =
    let a0 = P.fromIntegral a0'
        agmax = P.fromIntegral agmax'
        divisor = agmax D.- a0
    in  [: (P.fromIntegral freq' D.- a0) D./ divisor | freq' <- as :]

scale :: Int {-gmax-} -> Hist Double -> Hist Int
scale gmax as = [: P.floor (a D.* P.fromIntegral gmax) |  a <- as :]

apply :: Hist Int -> Image Int -> Image Int
apply as img = mapP (mapP (as !:)) img




