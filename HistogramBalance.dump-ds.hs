Result size of Desugar (after optimization)
  = {terms: 88, types: 101, coercions: 0}

apply :: AkkuHist Int -> Image Int -> Image Int
apply =
  \ (as :: AkkuHist Int) (img :: Image Int) ->
    mapP (mapP (!: as)) img

accu :: Hist Int -> AkkuHist Int
accu = scanlP + (I# 0)

hist :: Image Int -> Hist Int
hist =
  . (. (. (mapP sumP) (groupP $fEqInt)) (sortP $fOrdInt)) (concatP)

normalize :: Int -> Int -> AkkuHist Int -> AkkuHist Double
normalize =
  \ (a0' :: Int) (agmax' :: Int) (as :: AkkuHist Int) ->
    let {
      a0 :: Double
      a0 = fromIntegral $fIntegralInt $fNumDouble a0' } in
    let {
      divisor :: Double
      divisor = - a0 (fromIntegral $fIntegralInt $fNumDouble agmax') } in
    mapP
      (\ (ds :: Int) ->
         / (- (fromIntegral $fIntegralInt $fNumDouble ds) a0) divisor)
      as

scale :: Int -> AkkuHist Double -> AkkuHist Int
scale =
  \ (gmax :: Int) (as :: AkkuHist Double) ->
    mapP
      (\ (ds :: Double) ->
         floor
           $fRealFracDouble
           $fIntegralInt
           (* ds (fromIntegral $fIntegralInt $fNumDouble gmax)))
      as

hbalance :: Image Int -> Image Int
hbalance =
  \ (img :: Image Int) ->
    let {
      h :: Hist Int
      h = hist img } in
    let {
      a :: AkkuHist Int
      a = accu h } in
    apply
      (scale (- (lengthP h) (I# 1)) (normalize (headP a) (lastP a) a))
      img


