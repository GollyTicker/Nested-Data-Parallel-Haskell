{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PArray (PArray a)


-- Original context
context = (\h -> ...) (V[hist] $: img)
and
context = (\h -> ...) (L[hist] $:L img)

-- Original definition
hist :: Image Int -> Hist Int
hist = 
    sparseToDenseP (gmax+1) 0
    . mapP (\g -> (headP g,lengthP g))
    . groupP
    . sortP
    . concatP

    
-- desugared hist
hist0 :: Image Int -> Hist Int
hist0 =
  \img ->
    sparseToDenseP (gmax + 1) 0
      (mapP (\g -> (headP g, lengthP g))
        (groupP
          (sortP
            (concatP
              img
            )
          )
        )
      )


-- vectorized type
hist1 :: PAImage Int :-> PA Int
hist1 = 
  V[\img ->
      sparseToDenseP (gmax + 1) 0
        (mapP (\g -> (headP g, lengthP g))
          (groupP
            (sortP
              (concatP
                img
              )
            )
          )
        )
  ]

-- vectorized lambda
hist2 :: PAImage Int :-> PA Int
hist2 = 
  Clo () (\() img -> V[b]) (\(ATup0 n) img -> L[b] n)
  where b = 
          sparseToDenseP (gmax + 1) 0
            (mapP (\g -> (headP g, lengthP g))
              (groupP
                (sortP
                  (concatP
                    img
                  )
                )
              )
            )
  let hist3 = V[b]

hist3 :: PA Int
hist3 =
  V[
    sparseToDenseP (gmax + 1) 0
      (mapP (\g -> (headP g, lengthP g))
        (groupP
          (sortP
            (concatP
              img
            )
          )
        )
      )
  ]

-- vector apply
hist3 :: PA Int
hist3 =
  V[sparseToDenseP]
    $: (V[+] $: V[gmax] $: V[1])
    $: V[0]
    $: V[mapP (\g -> (headP g, lengthP g))
         (groupP
           (sortP
             (concatP
               img
             )
           )
         )
       ]

-- vector apply (more)
hist3 :: PA Int
hist3 =
  V[sparseToDenseP]
    $: (V[+] $: V[gmax] $: V[1])
    $: V[0]
    $: V[mapP]
       $: V[\g -> (,) (headP g) (lengthP g)]
       $: V[groupP]
          $: V[sortP]
             $: V[concatP]
                $: V[img]

-- vector function, lambda
hist3 :: PA Int
hist3 =
  sparseToDensePV
    $: (plusIntV $: gmax $: 1)
    $: 0
    $: mapPV
       $: Clo {
            env = ()
           ,scalar = (...ignored inside mapP...)
           ,lifted = \(ATup0 n) g -> (,)L (replPA n headPV $:L g) (replPA n lengthPV $:L g)
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
        sparseToDensePV
          $: (plusIntV $: gmax $: 1)
          $: 0
          $: mapPV
             $: Clo {
                  env = ()
                 ,scalar = (...ignored inside mapP...)
                 ,lifted =
                    \(ATup0 n) g ->
                      (,)L
                        (replPA n headPV $:L g)
                        (replPA n lengthPV $:L g)
                }
             $: groupPV
                $: sortPV
                   $: concatPV
                      $: img
    ,lifted = (...ignored in context...)
  }
  
