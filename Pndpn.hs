
"Pndpvn: Programmer implementation using NDP"

type Image = [:[:Int:]:]
type Hist a = [:a:]

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

hist :: Image -> Hist Int
hist = 
    sparseToDenseP (gmax+1) 0
    . mapP (\g -> (headP g,lengthP g))
    . groupP
    . sortP
    . concatP

accu :: Hist Int -> Hist Int
accu = scanlP (+) 0

normalize :: Int -> Int -> Hist Int -> Hist Double
normalize a0' agmax' as =
    let a0 = fromIntegral a0'
        agmax = fromIntegral agmax'
        divisor = agmax - a0
    in  [: (fromIntegral freq' - a0) / divisor | freq' <- as :]

scale :: Int -> Hist Double -> Hist Int
scale gmax as = [: floor (a * fromIntegral gmax) |  a <- as :]

apply :: Hist Int -> Image -> Image
apply as img = mapP (mapP (as !:)) img

{-

n sei die Anzahl der Bildpixel
w sei die Bildbreite
h sei die Bildhöhe

  Nested Laufzeiteinschätzung
  
Siehe PDF.
-}
