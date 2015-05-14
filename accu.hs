{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PArray (PArray a)


-- Original context
context = (\a -> ...) (V[accu] $: h)
context = (\a -> ...) (L[accu] n $:L h)

-- Original definition
-- (+) refers to double-multiplication
accu :: Hist Int -> AkkuHist Int
accu = scanlP (+) 0

-- desugared accu
accu0 :: AkkuHist Int -> AkkuHist Int
accu0 = \xs -> scanlP (\x -> \y -> x + y) 0 xs


-- vector type, lambda
accu0 :: PA Int :-> PA Int
accu0 =
  Clo {
     env = ()
    ,scalar = \() xs -> V[scanlP (\x -> \y -> x + y) 0 xs]
    ,lifted = (...ignored inside context...)
  }

-- vectorizing local expression
V[scanlP (\x -> \y -> x + y) 0 xs]
  -- vector apply
  = V[scanlP]
      $: V[\x -> \y -> x + y]
      $: V[0]
      $: V[xs]
  -- vector apply
  = scanlPV
      $: V[\x -> \y -> x + y]
      $: 0
      $: xs

-- vectorizing local expression
V[\x -> \y -> x + y]
  -- double vector lambda
  =
    Clo {
     env = ()
    ,scalar =
      \() x -> 
        Clo {
           env = (x)
          ,scalar = \(x) y -> V[(+) x y]
          ,lifted = (...ignored inside scanlP...)
        }
    ,lifted = (...ignored inside scanlP...)
  }

V[(+) x y]
  -- vector function, vector variables
  = plusIntV $: x $: y

-- final form before inlining
accu1 :: PA Int :-> PA Int
accu1 =
  Clo {
     env = ()
    ,scalar =
      \() xs ->
        scanlPV
          $: Clo {
               env = ()
              ,scalar =
                \() x -> 
                  Clo {
                     env = (x)
                    ,scalar = \(x) y -> plusIntV $: x $: y
                    ,lifted = (...ignored inside scanlP...)
                  }
              ,lifted = (...ignored inside scanlP...)
             }
          $: 0
          $: xs
    ,lifted = (...ignored inside context...)
  }

