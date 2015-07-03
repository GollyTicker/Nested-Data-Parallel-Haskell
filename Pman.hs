
"Pman: Programming with flat data parallelism"

type Image  = V.Vector Int   -- unboxed vector. aka dense heap array

type Hist   = V.Vector Int   -- der Index soll der Grauwert sein
                             -- und der enthaltene Wert das Ergebnis

hbalance :: Image -> Image
hbalance img =
  let h = parAccuHist img
      min = h[0]
      max = h[gmax]
      apply h = parMap (\i -> h[i]) img
      sclNrm :: Int -> Int
      sclNrm x = round( (x-min)/(max - min)*gmax )
  in  apply (parMap sclNrm) hist

gmax :: Int
gmax = 7 -- höchst möglicher Bildwert. in diesem Fall sind es 4 bit Bilder

parAccuHist :: Image -> Hist
parAccuHist []  = replicate gmax 0 -- [0,...,0]
parAccuHist [x] = generate gmax (\i -> if (i >= x) then 1 else 0 ) -- [0,...0,1,...,1], erste 1 ab index x
parAccuHist xs  =
  let (left,right) = splitMid xs
      [leftRes,rightRes] = parMap parAccuHist [left,right]  -- zwei parallele rek. Aufrufe
  in  parZipWith (+) leftRes rightRes

{-

n sei die Anzahl der Bildpixel
w sei die Bildbreite
h sei die Bildhöhe

       f          O(W)       O(D)
--------------------------------
  hbalance        n*gmax      log n
  parAccuHist     n*gmax      log n   (durch geometrische Folge oder Master-Theorem)
  apply           n           1
  parMap sclNrm   gmax        1
  
  splitMid        1           1
  parZipWith      gmax        1
  replicate       gmax        1
  generate        gmax        1
  parMap f xs     W(f,x)*size(xs)     1
  apply           n           1
  arr[i]          1           1
  

parMap      sei ein primitiv zum Bulk-Parallelen Ausführen einer Abbildung
parZipWith  sei ein primitiv zum elementweise-parallelen Abbilden zweier Arrays
parZipWith  :: (a -> b -> c) -> [a] -> [b] -> [c]
-}
