
"Pndpvn: Programmer implementation using NDP"

type Image = [:[: Int :]:]

type Hist a = [: a :]

hbalance :: Image -> Image
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
gmax = 255      -- der maximale Grauwert

hist :: Image -> Hist Int
hist = 
    sparseToDenseP (gmax+1) 0
    . mapP (\g -> (headP g,lengthP g))
    . groupP
    . sortP
    . concatP

accu :: Hist Int -> Hist Int
accu = scanlP (+) 0

normalize :: Int {-a0-} -> Int {-a(gmax)-} -> Hist Int -> Hist Double
normalize a0' agmax' as =
    let a0 = P.fromIntegral a0'
        agmax = P.fromIntegral agmax'
        divisor = agmax D.- a0
    in  [: (P.fromIntegral freq' D.- a0) D./ divisor | freq' <- as :]

scale :: Int {-gmax-} -> Hist Double -> Hist Int
scale gmax as = [: P.floor (a D.* P.fromIntegral gmax) |  a <- as :]

apply :: Hist Int -> Image -> Image
apply as img = mapP (mapP (as !:)) img

{-

n sei die Anzahl der Bildpixel
w sei die Bildbreite
h sei die Bildhöhe

  Nested Laufzeiteinschätzung
  
       f            O(W)                  O(D)
----------------------------------------------------------------
  hbalance          max(n * log n, gmax)  max(log n, log gmax)
  
  hist              max(n * log n, gmax)  log n
  sparseToDenseP    gmax                  1
  groupP            n                     log n
  sortP             n * log n             log n
  concatP           1                     1
  
  accu              gmax                  log gmax
  scanlP            gmax                  log gmax
  
  normalize         gmax                  1
  scale             gmax                  1
  
  apply             n                     1
                      = w*h*O(1)
  
  mapP f xs         W(f,x)*size(xs)       1
  headP/lastP       1                     1
  indexP, !:        1                     1
-}
