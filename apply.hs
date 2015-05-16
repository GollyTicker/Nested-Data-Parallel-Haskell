{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PArray (PArray a)


-- Original context
contextV = V[apply] $: (...) $: (...)
contextL = L[apply] n $:L (...) $:L (...)

-- Original definition
apply :: AkkuHist Int -> Image Int -> Image Int
apply as img = mapP (mapP (as !:)) img

-- desugared apply
apply0 :: AkkuHist Int -> Image Int -> Image Int
apply0 =
    \as ->
        \img ->
            mapP
                (\xs ->
                    mapP (\g -> as !: g) xs
                )
                img

-- Vectorized type
apply1 :: PA Int :-> PAImage Int :-> PAImage Int
apply1 =
  V[\as ->
        \img ->
            mapP
                (\xs ->
                    mapP (\g -> as !: g) xs
                )
                img
  ]

-- Vectorize lambda
apply2 :: PA Int :-> PAImage Int :-> PAImage Int
apply2 =
	Clo () (\() as -> V[b]) (_)
  where
  	b = \img ->
            mapP
                (\xs ->
                    mapP (\g -> as !: g) xs
                )
                img
	let apply3 = V[b]

apply3 :: PAImage Int :-> PAImage Int
apply3 = V[\img ->
            mapP
                (\xs ->
                    mapP (\g -> as !: g) xs
                )
                img]

-- Vectorize lambda (variable img. as is env)
apply3 :: PAImage Int :-> PAImage Int
apply3 =
	Clo (as) (\(as) img -> V[applyBody] ) (_)
	where b = mapP
							(\xs ->
							  mapP (\g -> as !: g) xs
							)
							img

applyBody = mapP
							(\xs ->
							  mapP (\g -> as !: g) xs
							)
							img

-- nested scalar versions ommited
V[applyBody]
  = mapPV
			$: Clo { env = (as)
					,lifted =
					  \(ATup1 n as) xs ->
					    replPA n mapPV
					      $:L AClo { aenv = ATup1 n as
			                ,lifted = \(ATup1 n as) g -> replicatePA n indexPV $:L as $:L g
		                }
                $:L xs
				 }
			$: img

L[applyBody] n
  = replPA n mapPV
			$:L AClo { env = ATup1 n as
					,alifted =
					  \(ATup1 n as) xs ->
					    replPA n mapPV
					      $:L AClo { aenv = ATup1 n as
			                ,lifted = \(ATup1 n as) g -> replicatePA n indexPV $:L as $:L g
		                }
                $:L xs
				 }
			$:L img

-- lifting locally
-- only lifted version will be needed, since it is an argument of mapP
-- replicatePA will be abbreviated as replPA
L[mapP (\g -> as !: g) xs] n
	-- Lifted apply, ($:L) is lifted application
	= L[mapP] n $:L L[\g -> as !: g] n $:L L[xs] n
	-- Lifted function, Lifted local variable
	= replPA n mapPV $:L L[\g -> as !: g] n $:L xs
	
L[\g -> as !: g] n
	-- Lifted Lambda
	= AClo {
			 aenv = (as)
			,scalar = (...ignored inside mapP...)
			,lifted = \(ATup1 n as) g -> L[as !: g] n
		}
	-- Lifted apply
	= AClo {
			 aenv = (as)
			,scalar = (...ignored inside mapP...)
			,lifted = \(ATup1 n as) g -> L[!:] n $:L L[as] n $:L L[g] n
		}
	-- Lifted function, Lifted variables
	= AClo {
			 aenv = (as)
			,scalar = (...ignored inside mapP...)
			,lifted = \(ATup1 n as) g -> replicatePA n indexPV $:L as $:L g
		}


{-            FINAL FORM OF APPLY IN CONTEXT        -}

contextV
  = V[apply] $: someAkku $: someImage
-- inline vertorized apply
  = Clo () (\() as -> Clo (as) (\(as) img -> V[applyBody] ) (_)) (_) $: someAkku $: someImage
-- definition of $: 2x
  = mapPV
			$: Clo { env = (someAkku)
					,lifted =
					  \(ATup1 n as) xs ->
					    replPA n mapPV
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
  = replPA n mapPV
			$:L AClo { env = ATup1 n someAkku
					,alifted =
					  \(ATup1 n as) xs ->
					    replPA n mapPV
					      $:L AClo { aenv = ATup1 n as
			                ,lifted = \(ATup1 n as) g -> replicatePA n indexPV $:L as $:L g
		                }
                $:L xs
				 }
			$:L someImage




