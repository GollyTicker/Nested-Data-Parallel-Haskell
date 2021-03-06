{-# LANGUAGE NoMonomorphismRestriction #-}

module ListHistogramBalance (main,hbalance,hbalanceBulk,img) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Prelude hiding (scanl,concat)
import qualified Data.List as L (intercalate,concat)
import qualified Data.Vector as V

main :: IO ()
main = do
  putStrLn "Original Image:"
  printImg img
  putStrLn "Balanced Image:"
  printImg (hbalance img)

type Image a  = V.Vector (V.Vector a)
type Many a   = V.Vector a


type Hist a = Map Int a

imgL :: [[Int]]
imgL =
  [
     [1,1,1,1,1,1]
    ,[1,6,6,1,1,1]
    ,[5,5,5,0,1,0]
    ,[5,1,1,0,1,0]
    ,[6,6,1,1,1,1]
   ]

img :: Image Int
img = V.map (V.fromList) . V.fromList $ imgL

images :: Many (Image Int)
images = V.replicate 1000 img

hbalanceBulk :: Many (Image Int) -> Many (Image Int)
hbalanceBulk = V.map hbalance

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
gmax = 7

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
apply as img = V.map (V.map (lookupLessEqual as)) img

lookupLessEqual as = (\i -> snd . fromJust $ M.lookupLE i as)

{-
mapAccum :: (a -> b -> (a,c)) -> a -> Map k b -> (a, Map k c)
-}
scanl :: (b -> c -> c) -> c -> Map k b -> Map k c
scanl f z = snd . M.mapAccum (\accu elem -> let x = f elem accu in (x,x)) z

concat :: V.Vector (V.Vector a) -> V.Vector a
concat = V.concatMap id

printImg :: Show a => Image a -> IO ()
printImg =
  putStrLn
  . L.intercalate "\n"
  . map (L.concat . map show)
  . map (V.toList) . V.toList


