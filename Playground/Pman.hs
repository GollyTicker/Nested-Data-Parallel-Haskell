
"Pman: Programming with flat data parallelism"

type Image  = V.Vector Int   -- unboxed vector. aka dense heap array
type Hist   = V.Vector Int   -- the index is the gray-value. its value the result

hbalance :: Image -> Image
hbalance img =
  let hist = parAccuHist img
      min = hist ! 0
      max = hist ! gmax
      apply hist = parMap (\i -> h ! i) img
      sclNrm :: Int -> Int
      sclNrm x = round( (x-min)/(max - min)*gmax )
  in  apply (parMap sclNrm) hist

parAccuHist :: Image -> Hist
parAccuHist []  = replicate gmax 0            -- creates [0,...,0]
parAccuHist [x] = generate gmax (\i -> if (i >= x) then 1 else 0 ) -- create [0,...0,1,...,1]
parAccuHist xs  =
  let (left,right) = splitMid xs
      [leftRes,rightRes] = parMap parAccuHist [left,right] -- two parallel recursive calls
  in  parZipWith (+) leftRes rightRes

{-


siehe PDF

parMap      sei ein primitiv zum Bulk-Parallelen Ausführen einer Abbildung
parZipWith  sei ein primitiv zum elementweise-parallelen Abbilden zweier Arrays
parZipWith  :: (a -> b -> c) -> [a] -> [b] -> [c]
-}
