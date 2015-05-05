{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PArray (PArray a)

-- Original version
hbalance :: Image Int -> Image Int
hbalance img =
    let h = hist img
        a = accu h
        a0 = headP a
        agmax = lastP a
        gmax = lengthP h - 1
        n = normalize a0 agmax a
        s = scale gmax n
        img' = apply s img
    in  img'

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
          (scale (lengthP h - 1) (normalize (headP a) (lastP a) a))
          img

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
              (scale (lengthP h - 1) (normalize (headP a) (lastP a) a))
              img
  ]

-- Vectorize lambda (bind new variable hbalance3)
hbalance2 :: PAImage Int :-> PAImage Int
hbalance2 =
  Clo () (\() img -> hbalance3) (\(ATup0 n) img -> hbalance3L n)

-- Vectorize let binding
hbalance3 :: PAImage Int -- die Variable img ist hier gebunden an das hbalance1
hbalance3 =
    (\h -> 
      V[
        let a :: AkkuHist Int
            a = accu h
        in  apply
              (scale (lengthP h - 1) (normalize (headP a) (lastP a) a))
              img
    ]) $: V[hist img]

-- Vectorize let binding
hbalance4 :: PAImage Int
hbalance4 =
    (\h -> 
      (\a -> 
        V[apply
            (scale (lengthP h - 1) (normalize (headP a) (lastP a) a))
            img
        ]) $: V[accu h]
    ]) $: V[hist img]
  
-- Vectorize function application
hbalance5 :: PAImage Int
hbalance5 =
    (\h -> 
      (\a -> 
        V[apply]
            $: V[(scale (lengthP h - 1) (normalize (headP a) (lastP a) a))]
            $: V[img]
        ])
          $: (V[accu] $: V[h])
    ])
      $: (V[hist] $: V[img])

-- Vectorize img variable (locally bound in hbalance1)
-- Vectorize h variable
-- bind to scale1
hbalance6 :: PAImage Int
hbalance6 =
    (\h -> 
      (\a -> 
        V[apply]
            $: scale1
            $: img
        ])
          $: (V[accu] $: h)
    ])
      $: (V[hist] $: img)

scale1 :: PA Int
scale1 = 
  V[scale
    (lengthP h - 1)
    (normalize
      (headP a)
      (lastP a)
      a)
  ]

-- Vector apply
scale2 :: PA Int
scale2 = 
  V[scale]
    $: V[(lengthP h - 1)]
    $: V[
          (normalize
            (headP a)
            (lastP a)
            a)
       ]

-- Vector apply
-- and complete vectorization of first argument.
-- minusV is the predefined vectorized IntMinus
scale3 :: PA Int
scale3 = 
  V[scale]
    $: (minusV $: (lengthPV $: h) $: 1)
    $: (V[normalize]
          $: V[headP a]
          $: V[lastP a]
          $: V[a])

-- final form before inlining.
scale4 :: PA Int
scale4 = 
  V[scale]
    $: (minusV $: (lengthPV $: h) $: 1)
    $: (V[normalize]
          $: headPV a
          $: lastPV a
          $: a)

-- Vectorized hbalance (not including vectorized user-functions like hist)
-- hbalance3L ist not being shown, since it is going to be
-- cut-off in scalar applications.
hbalance6 :: PAImage Int
hbalance6 =
  Clo {
     env = ()
    ,lifted = (\(ATup0 n) img -> hbalance3L n)
    ,scalar =
      (\() img ->
        (\h -> 
            (\a -> 
              V[apply]
                  $: (V[scale]
                        $: (minusV $: (lengthPV $: h) $: 1)
                        $: (V[normalize] $: headPV a $: lastPV a $: a)
                     )
                  $: img
              )
                $: (V[accu] $: h)
          )
            $: (V[hist] $: img)
    )
    }
    


