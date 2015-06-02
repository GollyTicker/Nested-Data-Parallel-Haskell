
import qualified Data.Vector.Unboxed as V
import Data.List (group,sort)

-- dense arrays
type Image a = V.Vector a
type Hist a = V.Vector a

main = hbalance img

hbalance :: Image Int -> IO (Image Int)
hbalance img =
  do
    
    let sorted  = countSortPar gmax img
    putStrLn $ " sorted = " ++ show sorted
    
    let a       = runLengthEncoding sorted
    putStrLn $ " a = " ++ show a
    
    let a0      = V.last a
    let agmax   = V.head a
    
    let gmap    = scale gmax . normalize a0 agmax $ a
    putStrLn $ " gmap = " ++ show gmap
    
    let img'    = apply gmap img
    putStrLn $ " img' = " ++ show img'
    
    return img'
      

gmax = 7

countSortPar :: Int -> Image Int -> Image Int
countSortPar k = V.fromList . sort . V.toList

runLengthEncoding :: Image Int -> Hist Int
runLengthEncoding =
  V.fromList
  . map length
  . group
  . V.toList

normalize :: Int -> Int -> Hist Int -> Hist Double
normalize a0' agmax' as =
    let a0 = fromIntegral a0'
        agmax = fromIntegral agmax'
        divisor = agmax - a0
    in  V.map (\freq' -> (fromIntegral freq' - a0) / divisor) as

scale :: Int -> Hist Double -> Hist Int
scale gmax as = V.map (\a -> floor (a * fromIntegral gmax)) as

apply as img = V.backpermute img as

-- ================================
img :: Image Int
img = V.fromList
      . concat
      $ [
           [1,1,1,1,1,1]
          ,[1,6,6,1,1,1]
          ,[5,5,5,0,1,0]
          ,[5,1,1,0,1,0]
          ,[6,6,1,1,1,1]
         ]

