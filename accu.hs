{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PArray (PArray a)


-- Original context
contextV = V[accu] $: h
contextL = L[accu] n $:L h

-- Original definition
-- (+) refers to double-multiplication
accu :: Hist Int -> AkkuHist Int
accu = scanlP (+) 0

-- desugared accu
accu0 :: [:Int:] -> [:Int:]
accu0 = \xs -> scanlP (+) 0 xs
{-
  Note: desugaring (+) to (\x -> \y -> x + y)
  is not nesessary. This is an optimization to
  reduce the number of steps in the vectorization/lifting.
-}


{-                LIFTED & VECTORIZED   ACCU      -}

L[accu] :: PA (PA Int :-> PA Int)
  = L[\xs -> scanlP (+) 0 xs]
  = AClo {
       aenv = ()
      ,ascalar = \() xs -> V[accuBody]
      ,alifted = \(ATup0 n) xs -> L[accuBody] n
    }

V[accu] :: PA Int :-> PA Int
  = V[\xs -> scanlP (+) 0 xs]
  = Clo {
       env = ()
      ,scalar = \() xs -> V[accuBody]
      ,lifted = \(ATup0 n) xs -> L[accuBody] n
    }

accuBody :: AkkuHist Int
accuBody = scanlP (+) 0 xs

{-                LIFTING & VECTORIZING ACCU BODY      -}

V[accuBody]
  = V[scanlP (+) 0 xs]
  = scanlPV $: plusIntV $: 0 $: xs
  
L[accuBody] n
  = L[scanlP (+) 0 xs] n
  = scanlPL $:L plusIntV $:L replPA n 0 $:L xs


{-            FINAL FORM WITH ACCU IN CONTEXT       -}

contextV
  = V[accu] $: h
  = (\xs -> scanlPV $: plusIntV $: 0 $: xs) $: h
  = scanlPV $: plusIntV $: 0 $: h

contextL n
  = L[accu] n $:L h
  = scanlPL $:L plusIntV $:L replPA n 0 $:L h



