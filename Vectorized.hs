
"Result of manual vectorization"

"$   and .   are application/composition of usual functions"
"$:  and .:  are scalar application/composition of vectorized functions"
"$:L and .:L are lifted application/composition of usual functions"

"replPA was rewritten to replPS to confirm to the notational standard
",because replPS form the paper refers to the scalar function"

"Inlining simple definitions"

V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusIntV 0    -- accu
            . sparseToDensePS (plusIntS gmax 1) 0   -- hist end
            . mapPS
               $ Clo { env = ()
                    ,lifted = \(ATup0 n) g -> (,)L (replPS n headPV $:L g) (replPS n lengthPV $:L g)
                  }
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img
    in  mapPS -- apply.
		      $ Clo {
				      lifted =
				        \(ATup1 n as) xs ->
				          replPS n mapPV  -- core of nested data parallelism here!
				            $:L AClo { aenv = ATup1 n as
		                      ,lifted = \(ATup1 n as) g -> replPS n indexPV $:L as $:L g   -- apply on every pixel
	                      }
                    $:L xs
		         ,env =  mapPS    -- scale
                       Clo { env = (gmax)
                         ,lifted =
                          \(ATup1 n gmax) a ->
                            replPS n floorV
                              $:L (replPS n multDoubleV $:L (replPS fromIntegralV n $:L gmax) $:L a)  -- scale each grayvalue
                        }
                     . mapPS   --  normalize, normalize every value in akku-histogram
                         Clo {
                             env = (int2Double (headPS a), minusDoubleS (int2Double (lastPS a)) a0)
                            ,lifted =
                              \(ATup2 n a0 divisior) a ->
                                 replPS n divV
                                   $:L replPS n minusV
                                         $:L (replPS n fromIntegralV $:L a)
                                         $:L a0
                                   $:L divisor
                          }
                     $ a
			       }
		      $ img

"Note: functions like concatPS' are simple concatPS (). This is nesessarly, since vectorized functions are passied their environment."


"Merging all definitions"
V[hbalance] $: img :: PA (PA Int)
  = (\h -> 
      (\a -> 
        mapPV -- apply.
			    $: Clo {
					    lifted =
					      \(ATup1 n as) xs ->
					        replPS n mapPV  -- core of nested data parallelism here!
					          $:L AClo { aenv = ATup1 n as
			                    ,lifted = \(ATup1 n as) g -> replicatePA n indexPV $:L as $:L g   -- apply on every pixel
		                    }
                    $:L xs
			       ,env = mapPV    -- scale
                     $: Clo { env = (gmax)
                         ,lifted =
                          \(ATup1 n gmax) a ->
                            replPS n floorV
                              $:L (replPS n multDoubleV $:L (replPS fromIntegralV n $:L gmax) $:L a)  -- scale each grayvalue
                        }
                     $: (\a0 ->           -- normalize
                          (\divisor -> 
                            mapPV   --  normalize every value in akku-histogram
                              $: Clo {
                                    env = (a0,divisior)
                                   ,lifted =
                                     \(ATup2 n a0 divisior) a ->
                                        replPS n divV
                                          $:L replPS n minusV
                                                $:L (replPS n fromIntegralV $:L a)
                                                $:L a0
                                          $:L divisor
                                 }
                              $: a
                          ) (minusDoubleV $: (fromIntegralV $: (lastPV $: a)) $: a0)
                        ) (fromIntegralV $: (headPV $: a))
				     }
			    $: img
        )
        $ (scanlPV $: plusIntV $: 0 $: h) -- accu
    ) $ sparseToDensePV -- hist
        $: (plusIntV $: gmax $: 1)
        $: 0
        $: mapPV
           $: Clo { env = ()
                ,lifted = \(ATup0 n) g -> ATup2 (replPS n headPV $:L g) (replPS n lengthPV $:L g)
              }
           $: groupPV
              $: sortPV
                 $: concatPV
                    $: img


"Collected from other files"

{-          HBALANCE            -}

resultS = V[hbalance] $: img
resultL = mapPV $: V[hbalance] $: imgs

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

{-          SCALE       -}
contextV
  = V[scale] $: someInt $: someNormHist
  = mapPV
      $: Clo {
           env = (someInt)
          ,lifted = \(ATup1 n gmax) a -> replPS n floorV $:L (replPS n multDoubleV $:L (replPS fromIntegralV n $:L gmax) $:L a)
          ,scalar = (...ignored inside mapP...)
         }
      $: someNormHist

contextL
  = L[scale] n $:L someInt $:L someNormHist
  = replPS n mapPV
      $:L AClo {
           aenv = ATup1 n someInt
          ,alifted = \(ATup1 n gmax) a -> replPS n floorV $:L (replPS n multDoubleV $:L (replPS fromIntegralV n $:L gmax) $:L a)
          ,scalar = (...ignored inside mapPV...)
         }
      $:L someNormHist


{-        ACCU      -}

contextV
  = V[accu] $: h
  = (\xs -> scanlPV $: plusIntV $: 0 $: xs) $: h
  = scanlPV $: plusIntV $: 0 $: h

contextL n
  = L[accu] n $:L h
  = scanlPL $:L plusIntV $:L replPS n 0 $:L h
  
{-      HIST        -}
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
  = replPS n sparseToDensePV
      $:L replPS n plusIntV $:L replPS n gmax $:L replPS n 1
      $:L replPS n 0
      $:L replPS n mapPV
          $:L AClo (_) _ lambdaGL
          $:L replPS n groupPV
              $:L replPS n sortPV
                  $:L replPS n concatPV
                      $:L img
lambdaGL
  = \(ATup0 n) g ->
          ATup2
            (replPS n headPV $:L g)
            (replPS n lengthPV $:L g)


{-        APPLY         -}
contextV
  = V[apply] $: someAkku $: someImage
-- inline vertorized apply
  = Clo () (\() as -> Clo (as) (\(as) img -> V[applyBody] ) (_)) (_) $: someAkku $: someImage
-- definition of $: 2x
  = mapPV
			$: Clo { env = (someAkku)
					,lifted =
					  \(ATup1 n as) xs ->
					    replPS n mapPV
					      $:L AClo { aenv = ATup1 n as
			                ,lifted = \(ATup1 n as) g -> replicatePA n indexPV $:L as $:L g
		                }
                $:L xs
				 }
			$: someImage

contextL n
  = L[apply] n $:L someAkku $:L someImage
-- inline vertorized apply, and definition of $:
  = AClo _ _ (\_ as -> AClo (ATup0 n as) _ (\(ATup0 n as) img -> L[applyBody])) $:L someAkku $:L someImage
-- definition of $:L 2x
  = replPS n mapPV
			$:L AClo { env = ATup1 n someAkku
					,alifted =
					  \(ATup1 n as) xs ->
					    replPS n mapPV
					      $:L AClo { aenv = ATup1 n as
			                ,lifted = \(ATup1 n as) g -> replicatePA n indexPV $:L as $:L g
		                }
                $:L xs
				 }
			$:L someImage



{-        NORMALIZE       -}
V[normalize] $: someA0 $: someAgmax $: someAccu =
  (\a0 ->
    (\divisor -> 
      mapPV
        $: Clo {
              env = (a0,divisior)
             ,scalar = (...ignored inside mapP...)
             ,lifted =
               \(ATup2 n a0 divisior) a ->
                  replPS n divV
                    $:L replPS n minusV
                          $:L (replPS n fromIntegralV $:L a)
                          $:L a0
                    $:L divisor
           }
        $: someAccu
    ) (minusDoubleV $: (fromIntegralV $: someAgmax) $: a0)
  ) (fromIntegralV $: someA0)

L[normalize] n $:L someA0 $:L someAgmax $:L someAccu =
  (\a0 ->
        (\divisor -> 
          replPS n mapPV
            $:L AClo {
                  aenv = ATup2 n a0 divisior
                 ,alifted =
                   \(ATup2 n a0 divisior) a ->
                      replPS n divV
                        $:L replPS n minusV
                              $:L (replPS n fromIntegralV $:L a)
                              $:L a0
                        $:L divisor
               }
            $:L someAccu
        ) (replPS n minusDoubleV $:L (replPS n fromIntegralV $:L someAgmax) $:L a0)
      ) (replPS n fromIntegralV $:L someA0)




