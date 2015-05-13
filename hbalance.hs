{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PA (PA a)

-- Original context
result = hbalance (... image ...)

result = mapP hbalance (... parallel array of images ...)

-- We therefore have to use to scalar and lifted version of the function.

-- Original version
hbalance :: Image Int -> Image Int
hbalance img =
    let h = hist img
        a = accu h
        a0 = headP a
        agmax = lastP a
        n = normalize a0 agmax a
        s = scale gmax n
        img' = apply s img
    in  img'

gmax :: Int
gmax = 255

-- vectorized gmax
gmax0 :: Int
gmax0 = 225


-- desugared.
hbalance0 :: Image Int -> Image Int
hbalance0 = \img ->
  let 
    h :: Hist Int
    h = hist img
  in
    let a :: AkkuHist Int
        a = accu h
    in  apply
          (scale gmax (normalize (headP a) (lastP a) a))
          img

{-                  HBALANCE                  -}

-- Vectorized type.
hbalance1 :: PAImage Int :-> PAImage Int
hbalance1 =
  V[\img ->
      let 
        h :: Hist Int
        h = hist img
      in
        let a :: AkkuHist Int
            a = accu h
        in  apply
              (scale gmax (normalize (headP a) (lastP a) a))
              img
  ]

-- Vectorize lambda (bind new variable hbalance3)
hbalance2 :: PAImage Int :-> PAImage Int
hbalance2 =
  Clo () (\() img -> hbalanceBody) (\(ATup0 n) img -> hbalanceBodyL n)

{-                    HBALANCE BODY      LIFTED         -}
hbalanceBodyL1 :: PA (PAImage Int)
hbalanceBodyL1 n =    -- TODO: continue here!
  L[let 
      h :: Hist Int
      h = hist img
    in
      let a :: AkkuHist Int
          a = accu h
      in  apply
            (scale gmax (normalize (headP a) (lastP a) a))
            img
  ] n

{-                    HBALANCE BODY      SCALAR         -}

-- Vectorize let binding
hbalanceBody1 :: PAImage Int -- die Variable img ist hier gebunden an das hbalance1
hbalanceBody1 =
    (\h -> 
      V[
        let a :: AkkuHist Int
            a = accu h
        in  apply
              (scale gmax (normalize (headP a) (lastP a) a))
              img
    ]) $: V[hist img]

-- Vectorize let binding
hbalanceBody2 :: PAImage Int
hbalanceBody2 =
    (\h -> 
      (\a -> 
        V[apply
            (scale gmax (normalize (headP a) (lastP a) a))
            img
        ]) $: V[accu h]
    ]) $: V[hist img]
  
-- Vectorize function application
hbalanceBody3 :: PAImage Int
hbalanceBody3 =
    (\h -> 
      (\a -> 
        V[apply]
            $: V[(scale gmax (normalize (headP a) (lastP a) a))]
            $: V[img]
        ])
          $: (V[accu] $: V[h])
    ])
      $: (V[hist] $: V[img])

-- Vectorize img variable (locally bound in hbalance1)
-- Vectorize h variable
-- bind to scale1
hbalanceBody4 :: PAImage Int
hbalanceBody4 =
    (\h -> 
      (\a -> 
        V[apply]
            $: scale1
            $: img
        ])
          $: (V[accu] $: h)
    ])
      $: (V[hist] $: img)



{-                      SCALE SCALAR              -}

scale1 :: PA Int
scale1 = 
  V[scale
    gmax
    (normalize
      (headP a)
      (lastP a)
      a)
  ]

-- Vector apply
scale2 :: PA Int
scale2 = 
  V[scale]
    $: V[gmax]
    $: V[
          (normalize
            (headP a)
            (lastP a)
            a)
       ]

-- Vector apply
-- Vector variable (top-level bound gmax)
scale3 :: PA Int
scale3 = 
  V[scale]
    $: gmax
    $: (V[normalize]
          $: V[headP a]
          $: V[lastP a]
          $: V[a])

-- final form before inlining.
scale4 :: PA Int
scale4 = 
  V[scale]
    $: gmax
    $: (V[normalize]
          $: headPV a
          $: lastPV a
          $: a)

{-                    FINAL FORM BEFORE OPTIMIZATION       -}

-- Vectorized hbalance (not including vectorized user-functions like hist)
-- hbalance3L ist not being shown, since it is going to be
-- cut-off in scalar applications.
hbalance3 :: PAImage Int
hbalance3 =
  Clo {
     env = ()
    ,lifted = (\(ATup0 n) img -> hbalanceBodyL n)
    ,scalar =
      (\() img ->
        (\h -> 
            (\a -> 
              V[apply]
                  $: (V[scale]
                        $: gmax
                        $: (V[normalize] $: headPV a $: lastPV a $: a)
                     )
                  $: img
              )
                $: (V[accu] $: h)
          )
            $: (V[hist] $: img)
    )
    }
    


