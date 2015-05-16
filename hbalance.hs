{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PA (PA a)

-- Original context
resultA = V[hbalance] $: V[... image ...]

resultB = mapPV $: V[hbalance] $: V[... parallel array of images ...]

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
hbalanceBodyL1 n =
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

-- let expression lifting
hbalanceBodyL2 :: PA (PAImage Int)
hbalanceBodyL2 n =
  (\h -> (L[lambdaA] n)) (L[hist img] n)
  where
    lambdaA = 
      let a :: AkkuHist Int
          a = accu h
      in  apply
            (scale gmax (normalize (headP a) (lastP a) a))
            img

-- let expression lifting
hbalanceBodyL2 :: PA (PAImage Int)
hbalanceBodyL2 n =
  (\h -> L[lambdaA] n) (L[hist] $:L img)
  where
    lambdaA = 
      let a :: AkkuHist Int
          a = accu h
      in  apply
            (scale gmax (normalize (headP a) (lastP a) a))
            img

-- let expression lifting
hbalanceBodyL3 :: PA (PAImage Int)
hbalanceBodyL3 n =
  (\h ->
    (\a -> 
      L[applyBody] n
    ) (L[accu] n $:L h)
  ) (L[hist] n $:L img)
  where
    applyBody =
      apply
        (scale gmax (normalize (headP a) (lastP a) a))
        img
  
{-                    HABALANCE BODY     APPLY           -}
L[applyBody]
  = L[apply]
      $:L L[scale gmax (normalize (headP a) (lastP a) a))]
      $:L L[img]
  = L[apply]
      $:L L[scale]
            $:L gmax
            $:L L[normalize]
                  $:L (headPL $:L a)
                  $:L (lastPL $:L a)
                  $:L a
      $:L img


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
    ]) V[hist img]

-- Vectorize let binding
hbalanceBody2 :: PAImage Int
hbalanceBody2 =
    (\h -> 
      (\a -> 
        V[apply
            (scale gmax (normalize (headP a) (lastP a) a))
            img
        ]) V[accu h]
    ]) V[hist img]
  
-- Vectorize function application
hbalanceBody3 :: PAImage Int
hbalanceBody3 =
    (\h -> 
      (\a -> 
        V[apply]
            $: V[(scale gmax (normalize (headP a) (lastP a) a))]
            $: V[img]
        ]) (V[accu] $: V[h])
    ]) (V[hist] $: V[img])

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
        ]) (V[accu] $: h)
    ]) (V[hist] $: img)



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
          $: headPV $: a
          $: lastPV $: a
          $: a)

{-                    FINAL FORM BEFORE OPTIMIZATION       -}
V[hbalance] :: PA (PA Int) :-> PA (PA Int)
  = Clo {
       env = ()
      ,scalar = V[hbalanceBody]
      ,lifted = L[hbalanceBody]
    }

V[hbalanceBody] :: () -> PA (PA Int) -> PA (PA Int)
  = \() img ->
      (\h -> 
        (\a -> 
          V[apply]
              $: (V[scale]
                    $: gmax
                    $: V[normalize]
                          $: (headPV $: a)
                          $: (lastPV $: a)
                          $: a
                 )
              $: img
          ) (V[accu] $: h)
      ) (V[hist] $: img)

L[hbalanceBody] :: PA () -> PA (PA (PA Int))  ->  PA (PA (PA Int))
  = \(ATup0 n) img ->
      (\h ->
        (\a -> 
          L[apply] n
            $:L L[scale] n
                  $:L gmax
                  $:L L[normalize] n
                        $:L (headPL $:L a)
                        $:L (lastPL $:L a)
                        $:L a
            $:L img
        ) (L[accu] n $:L h)
      ) (L[hist] n $:L img)



{-              INLINING DEFINITIONS            -}

-- let images :: PA (PA (PA Int)) be a parallel array of Images
resultB = mapPV $: V[hbalance] $: img
  -- definition of mapP and $:
  = mapP1 () V[hbalance] $: img
  -- definition of mapP1
  = Clo (V[hbalance]) mapPS mapPL $: img
  -- definition of $:
  = mapPS V[hbalance] img
  -- definition of V[hbalance]
  = mapPS (Clo () V[hbalanceBody] L[hbalanceBody]) img
  -- definition of mapPS
  = L[hbalanceBody] (replPA (lengthPA images) ()) img
  -- rewrite rule unitEnv
  = (\n img -> (...)) (lengthPA img) img
  
{-
Optimization steps in GHC: http://stackoverflow.com/questions/12653787/what-optimizations-can-ghc-be-expected-to-perform-reliably/12661937#12661937
* Strictness Alanysis / Immediate calculation
* Common Subexpression elimination
* Inlining
* Merged case expressions
* Communication Fusioning / Stream Fusioning 


-}


