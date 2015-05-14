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


{-                VECTORIZE HIST              -}

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
  where histBody = 
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
  let hist3 = V[histBody]
{-                LIFTED HIST                      -}
L[hist]
  = L[\img -> histBody]
  = AClo {
       aenv = ATup0 n
      ,ascalar = \() img -> V[histBody]
      ,alifted = \(ATup0 n) img -> L[histBody]
    }

{-                VECTORIZE HIST BODY              -}

V[histBody] :: PA Int
V[histBody] =
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
V[histBody] :: PA Int
V[histBody] =
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
V[histBody] :: PA Int
V[histBody] =
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
V[histBody] :: PA Int
V[histBody] =
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

{-                LIFTED HIST BODY            -}
L[histBody]
 =  sparseToDensePL
      $:L L[(+) $:L replPA n gmax $:L replPA n 1]
      $:L replPA n 0
      $: mapPL
          $:L L[\g -> (,) (headP g) (lengthP g)]
          $:L L[groupP]
              $:L L[sortP]
                  $:L L[concatP]
                      $:L L[img]
      ]
 =  sparseToDensePL
      $:L plusIntL $:L replPA n gmax $:L replPA n 1]
      $:L replPA n 0
      $:L mapPL
          $:L L[\g -> (,) (headP g) (lengthP g)]
          $:L groupPL
              $:L sortPL
                  $:L concatPL
                      $:L img
      ]
 = L[histBody]
 =  sparseToDensePL
      $:L plusIntL $:L replPA n gmax $:L replPA n 1]
      $:L replPA n 0
      $:L mapPL
          $:L AClo {
                 aenv = ATup0 n
                ,ascalar = (...ignored inside mapPL...)
                ,alifted = lambdaGL
              }
          $:L groupPL
              $:L sortPL
                  $:L concatPL
                      $:L img
      ]

{-                LIFTED LAMBDA G               -}

lambdaGL
  = L[lambdaG]
  = L[\g -> (,) (headP g) (lengthP g)]
  = \(ATup0 n) g ->
      (,)L
        (replPA n headPV $:L g)
        (replPA n lengthPV $:L g)


{-                FINAL FORMS BEFORE OPTIMIZATION        -}

V[hist] :: PAImage Int :-> PA Int
V[hist] = 
  Clo {
     env = ()
    ,scalar = V[histBody]
    ,lifted = (...ignored in context...)
  }

L[hist]
  = L[\img -> histBody]
  = AClo {
       aenv = ATup0 n
      ,ascalar = \() img -> V[histBody]
      ,alifted = \(ATup0 n) img -> L[histBody]
    }
