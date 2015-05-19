{-# LANGUAGE NoMonomorphismRestriction #-}

-- module MthreadedHistogramBalance (hbalance,hbalanceBulk,hist,img,accu,apply,scale,normalize) where

-- Define the functions each implementation has to provide

import qualified Data.Vector.Unboxed as V -- dense arrays
import qualified Data.Vector as VB        -- array of pointers
import Data.Vector.Strategies

import qualified ListHistogramBalance as L

-- Vector parallel of hbalance?
-- one has to chose between fast unboxed sequential vectors
-- and slightly-slower but parallel boxed vectors
-- inthis approach, the unboxed fast vectors were taken.

-- In this implementation each histogram calculation is
-- sequential (but fast using unbboxed arrays). But the
-- entire Collection of images can be efficiently parallized using parallel strategies.


type Image a  = V.Vector a   -- unboxed vector. aka dense heap array
type Many a   = VB.Vector a 

type Hist a     = V.Vector a   -- der Index soll der Grauwert sein
type AkkuHist a = V.Vector a   -- und der enthaltene Wert das Ergebnis

imgW = 6
imgH = 6
imgSize = imgW * imgH

img :: Image Int -- let img be an example image. 400x400 with some values
img = V.fromList . concat $ L.imgL

imgs :: Many (Image Int) -- let imgs be a collection of images for bulk operation
imgs = VB.replicate 1000 img

main :: IO ()
main =
  let nCores = 4
  in  mapM_ print $ VB.toList $ VB.take 5 $ hbalanceBulk nCores imgs

-- bulk application of HistrogramBalance
hbalanceBulk :: Int -> Many (Image Int) -> Many (Image Int)
hbalanceBulk cores imgs = (VB.map hbalance imgs) `using` (parVector cores)


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
gmax = 7 -- höchst möglicher Bildwert. in diesem Fall sind es 4 bit Bilder

hist :: Image Int -> Hist Int
hist =
  let init = V.replicate (gmax + 1) 0
      step g v = v `update` (g,1 + v V.! g)
  in  foldr step init . V.toList

update :: V.Vector Int -> (Int,Int) -> V.Vector Int
update v (i,res) = v `V.update` (V.singleton (i, res))

accu :: Hist Int -> AkkuHist Int
accu = V.scanl1 (+)

normalize :: Int -> Int -> AkkuHist Int -> AkkuHist Double
normalize a0' agmax' as =
    let a0 = fromIntegral a0'
        agmax = fromIntegral agmax'
        divisor = agmax - a0
    in  V.map (\freq' -> (fromIntegral freq' - a0) / divisor) as


scale :: Int -> AkkuHist Double -> AkkuHist Int
scale gmax = V.map (\d -> floor (d * fromIntegral gmax))

apply :: AkkuHist Int -> Image Int -> Image Int
apply as = V.map (as V.!)

