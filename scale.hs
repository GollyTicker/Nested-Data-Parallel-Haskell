{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]


-- Original context
contextV = V[scale] $: someInt $: someNormHist
contextL = L[scale] n $:L someInt $:L someNormHist
-- Note: someInt appears in scalar and lifted variant.
-- despite the equal variable name, both have different types signatures!

-- Original definition
-- (*) refers to double-multiplication
scale :: Int -> [:Double:] -> [:Int:]
scale gmax as = [: floor (a * fromIntegral gmax) |  a <- as :]

-- desugared scale
scale0 :: Int -> [:Double:] -> [:Int:]
scale0 =
  \gmax ->
    \as ->
      mapP
        (\a -> floor (fromIntegral gmax * a))
        as

{-                    VECTORIZED & LIFTED SCALE       -}

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
            $:L a

{-                  VECTORIZED SCALE           -}
V[scale] :: Int :-> PA Double :-> PA Int
V[scale] = 
  Clo {
     env = ()
    ,scalar = \() gmax -> V[body1]
    ,lifted = (...ignored in context...)
  }

V[body1] =
  Clo {
     env = (gmax)
    ,scalar = \(gmax) as -> V[body2]
    ,lifted = (...ignored in context...)
  }

V[body2] =
  mapPV
    $: Clo {
         env = (gmax)
        ,lifted = \(ATup1 n gmax) a -> L[body3]
        ,scalar = (...ignored inside mapP...)
       }

L[body3] =
  replPA floorV n
    $:L multDoubleV
        $:L replPA fromIntegralV n
            $:L gmax
        $:L a

{-            INLINING LIFTED & VECTORIZED SCALE              -}
with scale = 
        \gmax ->
          \as ->
            mapP
              (\a -> floor (fromIntegral gmax * a))
              as

contextV = V[scale] $: someInt $: someNormHist

contextL = L[scale] n $:L someInt $:L someNormHist

-- TODO: einsetzten
