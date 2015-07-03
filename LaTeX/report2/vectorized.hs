"Result of manual vectorization"

"$   and .   are application/composition of usual functions"
"$:  and $:L  are scalar and lifted application of vectorized functions"

V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusIntV 0                    -- accu
            . sparseToDensePS (plusIntS gmax 1) 0 -- hist
            . mapPS
               $ Clo { env = ()
                    ,lifted = \(ATup0 n) g -> (,)L (replPS n headPV $:L g) (replPS n lengthPV $:L g)
                  }
            . groupPS
            . sortPS
            . concatPS
            $ img
    in  mapPS                                     -- apply.
		      $ Clo {
				      lifted =
				        \(ATup1 n as) xs ->
				          replPS n mapPV                  -- core of nested data parallelism here!
				            $:L AClo { aenv = ATup1 n as  -- apply on every pixel
		                      ,lifted = \(ATup1 n as) g -> replPS n indexPV $:L as $:L g
	                      }
                    $:L xs
		         ,env =  mapPS                        -- scale
                       Clo { env = (gmax)
                         ,lifted =
                          \(ATup1 n gmax) a ->
                            replPS n floorV       -- scale each grayvalue
                              $:L (replPS n multDoubleV $:L (replPS fromIntegralV n $:L gmax) $:L a)
                        }
                     . mapPS                      -- normalize
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

