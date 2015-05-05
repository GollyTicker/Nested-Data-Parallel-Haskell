{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PArray (PArray a)


-- Original context
context = (\h -> ...) $: (V[hist] $: img)

-- Original definition
hist :: Image Int -> Hist Int
hist = 
    mapP sumP
    . groupP
    . sortP
    . concatP

-- desugared hist
hist0 :: Image Int -> Hist Int
hist0 =
  \img ->
    mapP (\x -> sumP x)
      (groupP
        (sortP
          (concatP
            img
          )
        )
      )


-- vectorized type
hist1 :: PAImage Int :-> PA Int
hist1 = 
  V[\img ->
      mapP (\x -> sumP x)
        (groupP
          (sortP
            (concatP
              img
            )
          )
        )
  ]

-- vectorized lambda
hist2 :: PAImage Int :-> PA Int
hist2 = 
  Clo () (\() img -> V[b]) (\(ATup0 n) img -> L[b] n)
  where b = 
          mapP (\x -> sumP x)
            (groupP
              (sortP
                (concatP
                  img
                )
              )
            )
  let hist3 = V[b]

hist3 :: PA Int
hist3 =
  V[
    mapP (\x -> sumP x)
      (groupP
        (sortP
          (concatP
            img
          )
        )
      )
  ]

-- vector apply
hist3 :: PA Int
hist3 =
  V[mapP]
    $:  V[(\x -> sumP x)]
    $:  V[(groupP
            (sortP
              (concatP
                img
              )
            )
        )]

-- vector apply (more)
hist3 :: PA Int
hist3 =
  V[mapP]
    $: V[(\x -> sumP x)]
    $: V[groupP]
        $: V[sortP]
            $: V[concatP]
                $: V[img]

-- vector function, lambda
hist3 :: PA Int
hist3 =
  mapPV
    $: Clo {
         env = ()
        ,scalar = (...ignored inside mapP...)
        ,lifted = \(ATup0 n) x -> replPA sumPV n $:L x
       }
    $: groupPV
        $: sortPV
            $: concatPV
                $: img


-- final form before inlining
hist4 :: PAImage Int :-> PA Int
hist4 = 
  Clo {
     env = ()
    ,scalar =
      \() img ->
        mapPV
          $: Clo {
               env = ()
              ,scalar = (...ignored inside mapP...)
              ,lifted = \(ATup0 n) x -> replPA sumPV n $:L x
             }
          $: groupPV
              $: sortPV
                  $: concatPV
                      $: img
    ,lifted = (...ignored in context...)
  }
  
