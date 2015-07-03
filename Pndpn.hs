
"Pndpvn: Programmier implementation using NDP"

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




