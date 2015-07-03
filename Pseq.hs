
"Pseq: Sequential implementation"

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

gmax :: Int
gmax = 255

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

{-

n sei die Anzahl der Bildpixel
w sei die Bildbreite
h sei die Bildhöhe

       f            O(W)                      O(D) (same as W, because of sequential implementation)
-----------------------------------------------------------------------------------------------------
  hbalance          max(n * log gmax, gmax)   "
  
  hist              n * log gmax              "
  accu              gmax                      "
  normalize         gmax                      "
  scale             gmax                      "
  apply             n * log gmax              "
                      = w * h * log gmax
  
  M.lookupLessEqual log gmax                  "
  M.insertWith      log gmax                  "
  M.empty           1                         "
  M.first           1                         "
  M.last            1                         "
  M.map             gmax                      "
  M.scanl           gmax                      "
  
  V.concat          n                         "
  V.foldr f z       n * log gmax              "
                      = O( n*W(f,gmax)+W(z) )
  
  V.map f xs        size(xs)*W(f,x)           "


M.first = snd . head . M.toAscList
M.last = snd . head . M.toDescList
  
-}

