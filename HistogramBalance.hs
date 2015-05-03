{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

-- The fully vectorized Module using Parallel Arrays

module HistogramBalance (
        hbalance
    ) where
 
import qualified Prelude as P
import Data.Array.Parallel hiding ((+),(-),(<),div)
import Data.Array.Parallel.Prelude hiding ((+),(-),(<),div)
import Data.Array.Parallel.Prelude.Int
import qualified Data.Array.Parallel.Prelude.Double as D

import Utils

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

(.) = (P..)
undefined = P.undefined

hbalance :: Image Int -> Image Int
hbalance img =
    let h = hist img
        a = accu h
        a0 = headP a
        agmax = lastP a
        gmax = lengthP h - 1
        n = normalize a0 agmax a
        s = scale gmax n
        img' = apply s img
    in  img'

hist :: Image Int -> Hist Int
hist = 
    mapP sumP
    . groupP
    . sortP
    . concatP

accu :: Hist Int -> AkkuHist Int
accu = scanlP (+) 0

normalize :: Int {-a0-} -> Int {-a(gmax)-} -> AkkuHist Int -> AkkuHist Double
normalize a0' agmax' as =
    let a0 = P.fromIntegral a0'
        agmax = P.fromIntegral agmax'
        divisor = a0 D.- agmax
    in  [: (P.fromIntegral freq' D.- a0) D./ divisor | freq' <- as :]

scale :: Int {-gmax-} -> AkkuHist Double -> AkkuHist Int
scale gmax as = [: P.floor (a D.* (P.fromIntegral gmax)) |  a <- as :]

apply :: AkkuHist Int -> Image Int -> Image Int
apply as img = mapP (mapP (\g -> as !: g )) img




