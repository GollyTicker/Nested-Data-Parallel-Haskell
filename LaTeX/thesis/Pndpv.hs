type Image = PA (PA Int)
type Hist  = PA Int

hbalance :: Image -> Image
hbalance img =
  let a :: Hist
      a = joinD
          . mapD (\(as,a) -> mapS (plusInt a) as)
          . propagateD plusInt 0
          . mapD (scanlS plusInt 0)
          . sparseToDenseD (plusInt gmax 1) 0
          . splitSparseD (plusInt gmax 1)
          . joinD
          . mapD tripletToATup2
          . segdSplitMerge 0
          . sortPS
          . concatPS
          $ img
              
      n :: Int
      n = lengthPS a
      
      a0, divisor, gmax' :: Double
      a0      = int2Double . headPS $ a
      divisor = minusDouble (int2Double (lastPS a))
                . int2Double . headPS $ a
      gmax'   = int2Double $ gmax
      
      normScale :: Int -> Int
      normScale = 
        floorDouble
        . (flip multDouble) gmax'
        . (flip divDouble) divisor
        . (flip minusDouble) a
        . int2Double
        
      as :: Hist
      as = joinD . mapD (mapS normScale) . splitD $ a
      
      pixelReplicate :: Hist -> PA Hist
      pixelReplicate = concatPS
                       . replPL (lengths (getSegd xs))
                       . replPS (lengthPS img)
      
  in unconcatPS img
     . indexPL (pixelReplicate as)
     . concatPS
     $ img

