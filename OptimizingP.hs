
"First step of optimization. Optimizing on parallel arrays" (after that comes distributed types and stream/communication fusioning)

"$   and .   are application/composition of usual functions"
"$:  and .:  are scalar application/composition of vectorized functions"
"$:L and .:L are lifted application/composition of usual functions"


"Inlining more complex definitions"

-- replication of functions
replPS n fv $:L a
  = fl (replPS n env) a
-- special case top level functions
  = fl (replPS n ()) a    -- definition of repl for () 
  = fl' a -- fl' is the direct lifted-version of the function

Note: indexPL :: PA a -> PA i -> PA a


V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusIntV 0    -- accu
            . sparseToDensePS (plusIntS gmax 1) 0   -- hist end
            . mapPS
               $ Clo { env = ()
                    ,lifted = \(ATup0 n) g -> (,)L (headPL g) (lengthPL g)
                  }
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img
    in  mapPS -- apply.
		      $ Clo {
				      lifted =
				        \(ATup1 n as) xs ->
				          mapPL                                             -- core of nested data parallelism here!
				            $ AClo { aenv = ATup1 n as
		                    ,lifted = \(ATup1 n as) g -> indexPL as g   -- apply on every pixel
	                    }
                    $ xs
		         ,env =  mapPS    -- scale
                       Clo { env = (gmax)
                         ,lifted =
                          \(ATup1 n gmax) a ->
                            floorL (multDoubleL (int2DoubleL gmax) a)  -- scale each grayvalue
                        }
                     . mapPS   -- normalize, normalize every value in akku-histogram
                         Clo {
                             env = (int2Double (headPS a), minusDoubleS (int2Double (lastPS a)) a0)
                            ,lifted =
                              \(ATup2 n a0 divisior) a ->
                                 divL (minusL (int2DoubleL a) a0) divisor
                          }
                     $ a
			       }
		      $ img

-- inlining definitions of mapPS

V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusIntV 0    -- accu
            . sparseToDensePS (plusIntS gmax 1) 0   -- hist end
            . (\(ATup0 n) g -> (,)L (headPL g) (lengthPL g)) (replPS (lengthPS g))
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img-- apply on every pixel-- core of nested data parallelism here!
    in  
    (\(ATup1 n as) xs -> mapPL (AClo { aenv = ATup1 n as, lifted = \(ATup1 n as) g -> indexPL as g }) xs)
      (replPS (lengthPS img)
            (\a' ->
                (\(ATup1 n gmax) a -> floorL (multDoubleL (int2DoubleL gmax) a)) (replPS (lengthPS a') gmax) a'
            ) (
               (\(ATup2 n a0 divisior) a -> divL (minusL (int2DoubleL a) a0) divisor) -- normalize, normalize every value in akku-histogram
                   (replPS (lengthPS a) (int2Double (headPS a), minusDoubleS (int2Double (lastPS a)) a0))
               a
              )
      )
      img
      
-- simplyfing, inlining

-- replicatePS for tuple2
replPS n a b = ATup2 n (replPS n a) (replPS n b)

-- float out
( f (let a = b in e)) = let a = b in f e -- if f doesn't contain any free occurences of a

-- constructor specialisation
(\(A b c) -> d) (A e f)
  = (\b c -> d) e f

-- float out (let n = length a to top-level)

-- length invariant for lifted functions
lengthPS (divL a b) = lengthPS a -- or lengthPS b

-- rewrite rule.
"length/replicate"
  forall n a.
  lengthPS (replicatePS n a) = n
-- used to rewrite lengthPS a' to lengthPS n !


-- replicatePS for tuple1
replPS n a b = ATup1 n (replPS n a) (replPS n b)

-- constructor specialisation


V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusIntV 0    -- accu
            . sparseToDensePS (plusIntS gmax 1) 0   -- hist end
            . (\g -> (,)L (headPL g) (lengthPL g))  -- ignored argument
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img
        n = length a
    in  -- apply on every pixel
    (\(ATup1 n as) xs -> mapPL (AClo { aenv = ATup1 n as, lifted = \(ATup1 n as) g -> indexPL as g }) xs)-- core of nested data parallelism here!
      (replPS
        (lengthPS img)
        $ floorL
            (multDoubleL (int2DoubleL (replPS n gmax)))
          . divL    -- normalize, normalize every value in akku-histogram
              (minusL (int2DoubleL a) (  replPS n (int2Double (headPS a))  ))
              (  replPS n (minusDoubleS (int2Double (lastPS a)) a0)  )
        )
      img

-- TODO: inline more of mapPL etc...
-- TODO: rewrites and special semantics of accumulator calculation


