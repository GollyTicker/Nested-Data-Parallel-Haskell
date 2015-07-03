module MthreadedHistogramBalance (hbalance,hbalanceBulk) where

import qualified Data.Vector.Unboxed as V -- dense arrays
import qualified Data.Vector as VB        -- array of pointers
import Data.Vector.Strategies

type Image a  = V.Vector a    -- flat pointerless image representation
type Many a   = VB.Vector a   -- pointer-based representation of the collection

type Hist a   = V.Vector a 

-- processor parallel application of histogram balancing over a collection of images
hbalanceBulk :: Int -> Many (Image Int) -> Many (Image Int)
hbalanceBulk cores imgs = (VB.map hbalance imgs) `using` (parVector cores)

-- single sequential histogram balancing
hbalance :: Image Int -> Image Int
hbalance img =
  let h = hist img
      a = accu h
      a0 = V.head a
      agmax = V.last a
      n = normalize a0 agmax a
      s = scale gmax n
      img' = apply s img
  in img'

gmax :: Int
gmax = 255

hist :: Image Int -> Hist Int
hist =
  let init = V.replicate (gmax + 1) 0
      step g v = update v (g,1 + v V.! g)
  in  foldr step init . V.toList

accu :: Hist Int -> Hist Int
accu = V.scanl1 (+)

normalize :: Int -> Int -> Hist Int -> Hist Double
normalize a0' agmax' as =
    let a0 = fromIntegral a0'
        agmax = fromIntegral agmax'
        divisor = agmax - a0
    in  V.map (\freq' -> (fromIntegral freq' - a0) / divisor) as

scale :: Int -> Hist Double -> Hist Int
scale gmax = V.map (\d -> floor (d * fromIntegral gmax))

apply :: Hist Int -> Image Int -> Image Int
apply as = V.map (as V.!)

