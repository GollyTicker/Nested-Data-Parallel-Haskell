{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PArray (PArray a)


-- Original context
context = applyV $: (V[scale] $: (...) $: (...))

-- Original definition
-- (*) refers to double-multiplication
scale :: Int -> AkkuHist Double -> AkkuHist Int
scale gmax as = [: floor (a * fromIntegral gmax) |  a <- as :]

-- desugared scale
scale0 :: Int -> AkkuHist Double -> AkkuHist Int
scale0 =
  \gmax ->
    \as ->
      mapP
        (\a -> floor (fromIntegral gmax * a))
        as


-- vectorized type
scale1 :: Int :-> PA Double :-> PA Int
scale1 =
  V[\gmax ->
      \as ->
        mapP
          (\a -> floor (fromIntegral gmax * a))
          as
  ]

-- vectorize both lambdas
scale1 :: Int :-> PA Double :-> PA Int
scale1 =
  Clo {
     env = ()
    ,scalar =
      \() gmax ->
        Clo {
           env = (gmax)
          ,scalar = \(gmax) as -> V[mapP (\a -> floor (fromIntegral gmax * a)) as]
          ,lifted = (...ignored in context...)
        }
    ,lifted = (...ignored in context...)
  }

-- vectorizing local expression
V[mapP (\a -> floor (fromIntegral gmax * a)) as]
  -- vector apply
  = V[mapP]
      $: V[(\a -> floor (fromIntegral gmax * a))]
      $: V[as]
  -- vector function ,vector variable, vector lambda
  = mapPV
      $: Clo {
           env = (gmax)
          ,scalar = (...ignored inside mapP...)
          ,lifted = \(ATup1 n gmax) a -> L[floor (fromIntegral gmax * a)] n
         }
      $: as

-- lift local expression
L[floor (fromIntegral gmax * a)] n
  -- lift function
  = replPA floorV n $:L L[(*) (fromIntegral gmax) a] n
  -- lift function
  = replPA floorV n $:L (multDoubleV $:L L[(fromIntegral gmax)] n $:L L[a] n)
  -- lift function, lift variables
  = replPA floorV n
      $:L multDoubleV
            $:L replPA fromIntegralV n
                  $:L gmax
            $:L a n

-- final form before inlining
scale2 :: Int :-> PA Double :-> PA Int
scale2 = 
  Clo {
     env = ()
    ,scalar =
      \() gmax ->
        Clo {
           env = (gmax)
          ,scalar =
            \(gmax) as ->
              mapPV
                $: Clo {
                     env = (gmax)
                    ,lifted =
                      \(ATup1 n gmax) a ->
                        replPA floorV n
                          $:L multDoubleV
                                $:L replPA fromIntegralV n
                                      $:L gmax
                                $:L a n
                                    $: as
                    ,scalar = (...ignored inside mapP...)
                   }
          ,lifted = (...ignored in context...)
        }
    ,lifted = (...ignored in context...)
  }


