
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

Note: indexPL :: PA (PA a) -> PA i -> PA a


V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusInt 0    -- accu
            . sparseToDensePS (plusInt gmax 1) 0   -- hist end
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
                            floorDoubleL (multDoubleL (int2DoubleL gmax) a)  -- scale each grayvalue
                        }                                                                                           TODO: check correct order of operations.
                     . mapPS   -- normalize, normalize every value in akku-histogram
                         Clo {
                             env = (int2Double (headPS a), minusDouble (int2Double (lastPS a)) (int2Double (headPS a)))
                            ,lifted =
                              \(ATup2 n a0 divisior) a ->
                                 divL (minusL (int2DoubleL a) a0) divisor
                          }
                     $ a
			       }
		      $ img

-- inlining definitions of mapPS

V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusInt 0    -- accu
            . sparseToDensePS (plusInt gmax 1) 0   -- hist end
            . (\(ATup0 n) g -> ATup2 (headPL g) (lengthPL g)) (replPS (lengthPS g))
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img-- apply on every pixel-- core of nested data parallelism here!
    in  
    (\(ATup1 n as) xs -> mapPL (AClo { aenv = ATup1 n as, lifted = \(ATup1 n as) g -> indexPL as g }) xs)
      (replPS (lengthPS img)
            (\a' ->
                (\(ATup1 n gmax) a -> floorDoubleL (multDoubleL (int2DoubleL gmax) a) ) (replPS (lengthPS a') gmax) a'
            ) (
               (\(ATup2 n a0 divisior) a -> divL (minusL (int2DoubleL a) a0) divisor) -- normalize, normalize every value in akku-histogram
                   (replPS (lengthPS a) (int2Double (headPS a), minusDouble (int2Double (lastPS a)) (int2Double (headPS a))))
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

-- inline mapPL
mapPL = \(AClo aenv _ fl) ass -> unconcatPS ass . fl (replsPS (getSegd ass) aenv) . concatPS $ ass

-- context
(\(ATup1 n as) xs ->
  mapPL (AClo { aenv = ATup1 n as, lifted = \(ATup1 n as) g -> indexPL as g }) xs
)
  -- inline mapPL
  = (\(ATup1 n as) xs ->
      unconcatPS xs . (\(ATup1 n as) g -> indexPL as g) (replsPS (getSegd xs) (ATup1 n as)) . concatPS $ xs
    ) -- could be further optimized ...

replPS :: Segd -> PA a -> PA a -- aus Tipps.txt
replPS segd = concatPS . replPL (lengths segd)   -- kann weiter durch inlining optimiert werden, da nur die Daten genommen werden.

V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusInt 0    -- accu
            . sparseToDensePS (plusInt gmax 1) 0   -- hist end
            . (\g -> ATup2 (headPL g) (lengthPL g))  -- ignored argument
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img
        n = length a
    in  -- apply on every pixel -- core of nested data parallelism here!
    (\(ATup1 n as) xs ->
      unconcatPS xs . (\(ATup1 n as) g -> indexPL as g) (replsPS (getSegd xs) (ATup1 n as)) . concatPS $ xs
    ) (replPS
        (lengthPS img)
        $ floorDoubleL
          . multDoubleL (int2DoubleL (replPS n gmax))
          . divL    -- normalize, normalize every value in akku-histogram
              (minusL (int2DoubleL a) (  replPS n (int2Double (headPS a))  ))
              (  replPS n (minusDouble (int2Double (lastPS a)) (int2Double (headPS a)))  )
      )
      img

"apply lambda"

V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusInt 0    -- accu
            . sparseToDensePS (plusInt gmax 1) 0   -- hist end
            . (\g -> ATup2 (headPL g) (lengthPL g))  -- ignored argument
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img
        n = length a
        gs = floorDoubleL                                     -- normalize and scale
             . multDoubleL (int2DoubleL (replPS n gmax))
             . divL
                 (minusL (int2DoubleL a) (  replPS n (int2Double (headPS a))  ))
             . replPS n
             $ minusDouble (int2Double (lastPS a)) (int2Double (headPS a))
    in (\xss -> -- apply on every pixel -- core of nested data parallelism here!
         unconcatPS xs . indexPL (expandPS xss gs) . concatPS $ xss
       ) img

note:
 Der Ausdruck (concatPS . replPL (lengths (getSegd xs)) as) sorgt lediglich dafür,
 dass der bereits einmal senkrecht-replizierte AkkumulatorArray nochmal waagerecht-repliziert wird.
 Damit steht jedem Pixel eine direkte Kopie des gesamten Akkumulators zur Verfügung.
 Durch "Work Efficient Vectorization" kann diese Replikation effizienter gemacht werden.

