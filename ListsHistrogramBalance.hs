{-# LANGUAGE NoMonomorphismRestriction #-}

module ListHistogramBalance (
        main,
        hbalance
    ) where


import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Prelude hiding (scanl)
import Data.List (intercalate)

type Image a = [[a]]
type Hist a = Map Int a
type AkkuHist a = Map Int a

img :: Image Int
img =
  [
     [1,1,1,1,1,1]
    ,[1,6,6,1,1,1]
    ,[5,5,5,0,1,0]
    ,[5,1,1,0,1,0]
    ,[6,6,1,1,1,1]
   ]

main :: IO ()
main = do
  putStrLn "Original Image:"
  printImg img
  putStrLn "Balanced Image:"
  printImg (hbalance img)
  
images :: [Image Int]
images = replicate 1000 img

work :: (Image Int, [Image Int])
work = (hbalance img, map hbalance images)

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
gmax = 7 -- höchst möglicher Bildwert. in diesem Fall sind es 4 bit Bilder

hist :: Image Int -> Hist Int
hist = foldr (\i -> M.insertWith (+) i 1) M.empty . concat

accu :: Hist Int -> AkkuHist Int
accu = scanl (+) 0

normalize :: Int -> Int -> AkkuHist Int -> AkkuHist Double
normalize a0' agmax' as =
    let a0 = fromIntegral a0'
        agmax = fromIntegral agmax'
        divisor = agmax - a0
    in  M.map (\freq' -> (fromIntegral freq' - a0) / divisor) as

scale :: Int -> AkkuHist Double -> AkkuHist Int
scale gmax = M.map (\d -> floor (d * fromIntegral gmax))

apply :: AkkuHist Int -> Image Int -> Image Int
apply as img = map (map (as M.!)) img

--

{-
mapAccum :: (a -> b -> (a,c)) -> a -> Map k b -> (a, Map k c)
-}
scanl :: (b -> c -> c) -> c -> Map k b -> Map k c
scanl f z = snd . M.mapAccum (\accu elem -> let x = f elem accu in (x,x)) z


printImg :: Show a => Image a -> IO ()
printImg = putStrLn . intercalate "\n" . map (concat . map show)

