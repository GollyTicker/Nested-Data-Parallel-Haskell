{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]

type PAImage a = PArray (PArray a)


-- Original context
contextV = V[hist] $: img
contextL = L[hist] $:L img

-- Original definition
hist :: Image Int -> Hist Int
hist = 
    sparseToDenseP (gmax+1) 0
    . mapP (\g -> (headP g,lengthP g))
    . groupP
    . sortP
    . concatP

    
-- desugared hist
hist0 :: [:[:Int:]:] -> Hist Int
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
hist1 :: PA (PA Int)) :-> PA Int
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
           ,lifted = \(ATup0 n) g -> ATup2 (replPA n headPV $:L g) (replPA n lengthPV $:L g)
          }
       $: groupPV
          $: sortPV
             $: concatPV
                $: img

{-                LIFTED HIST BODY            -}
L[histBody]
 = replPA n sparseTodensePV
      $:L L[(+) $:L replPA n gmax $:L replPA n 1]
      $:L replPA n 0
      $:L replPA n mapPV
          $:L L[\g -> (,) (headP g) (lengthP g)]
          $:L L[groupP]
              $:L L[sortP]
                  $:L L[concatP]
                      $:L L[img]
      ]
 = replPA n sparseTodensePV
      $:L replPA n plusIntV $:L replPA n gmax $:L replPA n 1
      $:L replPA n 0
      $:L replPA n mapPV
          $:L L[\g -> (,) (headP g) (lengthP g)]
          $:L replPA n groupPV
              $:L replPA n sortPV
                  $:L replPA n concatPV
                      $:L img
      ]
 = L[histBody] :: PA (PA Int)
 = replPA n sparseTodensePV
      $:L replPA n plusIntV $:L replPA n gmax $:L replPA n 1
      $:L replPA n 0
      $:L replPA n mapPV
          $:L AClo {
                 aenv = ATup0 n
                ,ascalar = (...ignored inside mapPV...)
                ,alifted = lambdaGL
              }
          $:L replPA n groupPV
              $:L replPA n sortPV
                  $:L replPA n concatPV
                      $:L replPA n img
      ]

{-                LIFTED LAMBDA G               -}
lambdaGL
  = L[lambdaG]
  = L[\g -> (,) (headP g) (lengthP g)]
  = \(ATup0 n) g ->
      ATup2
        (replPA n headPV $:L g)
        (replPA n lengthPV $:L g)


{-                FINAL FORMS BEFORE OPTIMIZATION        -}

V[hist] :: PA (PA Int) :-> PA Int
  = Clo {
       env = ()
      ,scalar = V[histBody]
      ,lifted = (...ignored in context...)
    }

L[hist] :: PA ( PA (PA Int) :-> PA Int )
  = AClo {
       aenv = ATup0 n
      ,ascalar = (...ignored in context...)
      ,alifted = \(ATup0 n) img -> L[histBody]
    }


{-              FINAL FORM WITH HIST IN CONTEXT            -}

contextV
  = V[hist] $: img
  = sparseToDensePV
      $: (plusIntV $: gmax $: 1)
      $: 0
      $: mapPV
         $: Clo () _ lambdaGL
         $: groupPV
            $: sortPV
               $: concatPV
                  $: img

contextL
  = L[hist] n $:L img
  = replPA n sparseToDensePV
      $:L replPA n plusIntV $:L replPA n gmax $:L replPA n 1
      $:L replPA n 0
      $:L replPA n mapPV
          $:L AClo (_) _ lambdaGL
          $:L replPA n groupPV
              $:L replPA n sortPV
                  $:L replPA n concatPV
                      $:L img

