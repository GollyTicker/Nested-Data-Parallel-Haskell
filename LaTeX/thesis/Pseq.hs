type Image  = V.Vector (V.Vector Int)
type Hist a = Map Int a

hbalance :: Image -> Image
hbalance img =
  let h = hist img
      a = accu h
      a0 = M.first a
      agmax = M.last a
      n = normalize a0 agmax a
      s = scale gmax n
      img' = apply s img
  in img'

hist :: Image -> Hist Int
hist = V.foldr (\i -> M.insertWith (+) i 1) M.empty . V.concat

accu :: Hist Int -> Hist Int
accu = M.scanl (+) 0

normalize :: Int -> Int -> Hist Int -> Hist Double
normalize a0' agmax' as =
    let a0 = fromIntegral a0'
        agmax = fromIntegral agmax'
        divisor = agmax - a0
    in  M.map (\freq' -> (fromIntegral freq' - a0) / divisor) as

scale :: Int -> Hist Double -> Hist Int
scale gmax = M.map (\d -> floor (d * fromIntegral gmax))

apply :: Hist Int -> Image -> Image
apply as img = V.map (V.map (M.lookupLessEqual as)) img
