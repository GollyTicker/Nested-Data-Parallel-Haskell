module HistogramBalance (hbalance, hbalanceBulk) where

import qualified Prelude as P
import Data.Array.Parallel -- work of Chakravarty, Leshchinskiy, Jones, Keller and Marlow

type Many a  = [: a :]
type Image a = [:[: a :]:]

type Hist a = [: a :]

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
gmax = 255

hist :: Image Int -> Hist Int
hist = 
    sparseToDenseP (gmax+1) 0
    . mapP (\g -> (headP g,lengthP g))
    . groupP
    . sortP
    . concatP

accu :: Hist Int -> Hist Int
accu = scanlP (+) 0

normalize :: Int -> Int -> Hist Int -> Hist Double
normalize a0' agmax' as =
    let a0 = P.fromIntegral a0'
        agmax = P.fromIntegral agmax'
        divisor = agmax D.- a0
    in  [: (P.fromIntegral freq' D.- a0) D./ divisor | freq' <- as :]

scale :: Int -> Hist Double -> Hist Int
scale gmax as = [: P.floor (a D.* P.fromIntegral gmax) |  a <- as :]

apply :: Hist Int -> Image Int -> Image Int
apply as img = mapP (mapP (as !:)) img

