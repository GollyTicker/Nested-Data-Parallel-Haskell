{-# LANGUAGE NoMonomorphismRestriction #-}

module ExplainedListHistogramBalance (
        hbalance
    ) where


import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Prelude hiding (scanl)
import Data.List (intercalate)

type Image a = [[a]]
type Hist a = Map Int a

img :: Image Int
img =
  [
     [1,1,1,1,1,1]
    ,[1,6,6,1,1,1]
    ,[5,5,5,0,1,0]
    ,[5,1,1,0,1,0]
    ,[6,6,1,1,1,1]
   ]

expectedImage :: Image Int
expectedImage = 
  [
     [4,4,4,4,4,4]
    ,[4,7,7,4,4,4]
    ,[5,5,5,0,4,0]
    ,[5,4,4,0,4,0]
    ,[7,7,4,4,4,4]
  ]

gmax :: Int
gmax = 7 -- höchst möglicher Bildwert. in diesem Fall sind es 4 bit Bilder

printImg :: Show a => Image a -> IO ()
printImg = putStrLn . intercalate "\n" . map (concat . map show)

main :: IO ()
main = do
  img' <- hbalance img
  if img' == expectedImage
    then putStrLn "Balancing correct."
    else putStrLn "  ## Balancing incorrect! ##  "

hbalance :: Image Int -> IO (Image Int)
hbalance img =
  do
    putStrLn "Original Image:"
    printImg img
    let h = hist img
    putStrLn $ "Histogram: " ++ show h
    let a = accu h
    putStrLn $ "AccuHistogram: " ++ show a
    let a0 = snd $ head $ M.toAscList a
    putStrLn $ "Freq of lowest Grayvalue: " ++ show a0
    let agmax = snd $ head $ M.toDescList a
    putStrLn $ "Sum of all points: " ++ show agmax
    putStrLn $ "Highest possible Grayvalue: " ++ show gmax
    let n = normalize a0 agmax a
    putStrLn $ "Normalized Map: " ++ show n
    print n
    let s = scale gmax n
    putStrLn $ "Scaled Map: " ++ show s
    let img' = apply s img
    putStrLn "Balanced Image:"
    printImg img'
    return img'


hist :: Image Int -> Hist Int
hist = foldr (\i -> M.insertWith (+) i 1) M.empty . concat

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
apply as img = map (map (lookupLessEqual as)) img

lookupLessEqual as = (\i -> snd . fromJust $ M.lookupLE i as)

{-
mapAccum :: (a -> b -> (a,c)) -> a -> Map k b -> (a, Map k c)
-}
scanl :: (b -> c -> c) -> c -> Map k b -> Map k c
scanl f z = snd . M.mapAccum (\accu elem -> let x = f elem accu in (x,x)) z

