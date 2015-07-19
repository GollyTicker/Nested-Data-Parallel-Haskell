type Image  = V.Vector Int
type Hist   = V.Vector Int

hbalance :: Image -> Image
hbalance img =
  let hist = parAccuHist img
      min = hist ! 0
      max = hist ! gmax
      apply hist = parMap (\i -> h ! i) img
      sclNrm :: Int -> Int
      sclNrm x = floor ( (x-min)/(max - min)*gmax )
  in  apply (parMap sclNrm) hist

parAccuHist :: Image -> Hist
parAccuHist []  = replicate gmax 0
parAccuHist [x] =
  generate gmax (\i -> if (i >= x) then 1 else 0)
parAccuHist xs  =
  let (left,right) = splitMid xs
      [leftRes,rightRes] = parMap parAccuHist [left,right]
  in  parZipWith (+) leftRes rightRes

