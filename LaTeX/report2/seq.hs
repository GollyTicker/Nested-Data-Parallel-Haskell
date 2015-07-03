module ListHistogramBalance (hbalance,hbalanceBulk) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Vector as V

type Image a  = V.Vector (V.Vector a)
type Many a   = V.Vector a

type Hist a = Map Int a

-- apply histogram balancing on many images by applying it on each image
hbalanceBulk :: Many (Image Int) -> Many (Image Int)
hbalanceBulk = V.map hbalance

-- histogram balancing
hbalance :: Image Int -> Image Int
hbalance img =
  let h = hist img
      a = accu h
      a0 = snd $ head $ M.toAscList a
      agmax = snd $ head $ M.toDescList a
      n = normalize a0 agmax a
      s = scale gmax n
      img' = apply s img
  in img'

gmax :: Int
gmax = 255

hist :: Image Int -> Hist Int
hist = V.foldr (\i -> M.insertWith (+) i 1) M.empty . concat

accu :: Hist Int -> Hist Int
accu = scanl (+) 0

normalize :: Int -> Int -> Hist Int -> Hist Double
normalize a0' agmax' as =
    let a0 = fromIntegral a0'
        agmax = fromIntegral agmax'
        divisor = agmax - a0
    in  M.map (\freq' -> (fromIntegral freq' - a0) / divisor) as

scale :: Int -> Hist Double -> Hist Int
scale gmax = M.map (\d -> floor (d * fromIntegral gmax))

apply :: Hist Int -> Image Int -> Image Int
apply as img = V.map (V.map (as M.!)) img
