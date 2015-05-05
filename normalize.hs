{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PArray (PArray a)


-- Original context
context = (...) $: (V[normalize] $: (...) $: (...) $: (...))

-- Original definition
-- (-) and (/) refer to double-substraction and double-division
normalize :: Int -> Int -> AkkuHist Int -> AkkuHist Double
normalize a0' agmax' as =
    let a0 = fromIntegral a0'
        agmax = fromIntegral agmax'
        divisor = agmax - a0
    in  [: (fromIntegral freq' - a0) / divisor | freq' <- as :]

-- desugared normalize
normalize0 :: Int -> Int -> AkkuHist Int -> AkkuHist Double
normalize0 =
  \a0' ->
    \agmax' ->
      \as ->
        let a0 = fromIntegral a0'
        in  let divisor = (fromIntegral agmax') - a0
            in  mapP
                  (\a ->
                     (fromIntegral a - a0) / divisor
                  )
                  as


-- vector type, triple  lambda
normalize1 :: Int :-> Int :-> PA Int :-> PA Double
normalize1 =
  Clo {
     env = ()
    ,scalar = \() a0' ->
      Clo {
         env = (a0)
        ,scalar = \(a0) agmax' ->
          Clo {
             env = (a0,agmax)
            ,scalar = \(a0,agmax) as ->
              V[
                let a0 = fromIntegral a0'
                in  let divisor = (fromIntegral agmax') - a0
                    in  mapP
                          (\a ->
                             (fromIntegral a - a0) / divisor
                          )
                          as
              ]
            ,lifted = (...ignored inside context...)
          }
        ,lifted = (...ignored inside context...)
      }
    ,lifted = (...ignored inside context...)
  }

-- vectorize double let-binding
normalize1 :: Int :-> Int :-> PA Int :-> PA Double
normalize1 =
  Clo {
     env = ()
    ,scalar = \() a0' ->
      Clo {
         env = (a0)
        ,scalar = \(a0) agmax' ->
          Clo {
             env = (a0,agmax)
            ,scalar = \(a0,agmax) as ->
              (\a0 ->
                (\divisor -> 
                  V[mapP
                      (\a -> (fromIntegral a - a0) / divisor)
                      as
                  ]                                   -- 1
                ) $: V[(fromIntegral agmax') - a0]    -- 2
              ) $: V[fromIntegral a0']                -- 3
            ,lifted = (...ignored inside context...)
          }
        ,lifted = (...ignored inside context...)
      }
    ,lifted = (...ignored inside context...)
  }

--vectorize local expressions

-- vectorize local expression 1
V[mapP
    (\a -> (fromIntegral a - a0) / divisor)
    as
]
  -- vector apply, function
  = mapPV
      $: V[\a -> (fromIntegral a - a0) / divisor]
      $: V[as]
  -- vector variable, lambda
  = mapPV
      $:  Clo {
             env = (a0,divisior)
            ,scalar = (...ignored inside mapP...)
            ,lifted = \(ATup2 n a0 divisior) a -> L[(fromIntegral a - a0) / divisor] n
          }
      $: as

-- lift local expression
L[(fromIntegral a - a0) / divisor] n
  -- lift function, variables, lift application
  = replPA n divV
      $:L replPA n minusV
            $:L (replPA n fromIntegralV $:L a)
            $:L a0
      $:L divisor

-- vectorize local expression 2
V[(fromIntegral agmax') - a0]
  = minusDoubleV $: (fromIntegralV $: agmax') $: a0

-- vectorize local expression 3
V[fromIntegral a0]
  = fromIntegralV $: a0


-- final form before inlining
normalize2 :: Int :-> Int :-> PA Int :-> PA Double
normalize2 =
  Clo {
     env = ()
    ,scalar = \() a0' ->
      Clo {
         env = (a0)
        ,scalar = \(a0) agmax' ->
          Clo {
             env = (a0,agmax)
            ,scalar = \(a0,agmax) as ->
              (\a0 ->
                (\divisor -> 
                  mapPV
                    $: Clo {
                          env = (a0,divisior)
                         ,scalar = (...ignored inside mapP...)
                         ,lifted =
                           \(ATup2 n a0 divisior) a ->
                              replPA n divV
                                $:L replPA n minusV
                                      $:L (replPA n fromIntegralV $:L a)
                                      $:L a0
                                $:L divisor
                       }
                    $: as
                ) $: (minusDoubleV $: (fromIntegralV $: agmax') $: a0)
              ) $: (fromIntegralV $: a0)
            ,lifted = (...ignored inside context...)
          }
        ,lifted = (...ignored inside context...)
      }
    ,lifted = (...ignored inside context...)
  }







