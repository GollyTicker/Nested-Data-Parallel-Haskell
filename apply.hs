{-# LANGUAGE ParallelArrays #-}

-- Manual vectorization of hbalance

type Image a = [:[: a :]:]
type Hist a = [: a :]
type AkkuHist a = [: a :]

type PAImage a = PArray (PArray a)


-- Original context
context = V[apply] $: (...) $: (...)

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
	Clo () (\() as -> V[b]) (\(ATup0 n) as -> L[b] n)
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
	Clo (as) (\(as) img -> V[b]) (\(ATup1 n as) img -> L[b] n)
	where b = mapP
							(\xs ->
							  mapP (\g -> as !: g) xs
							)
							img
	let apply4 = V[b]

apply4 :: PAImage Int
apply4 = V[mapP
							(\xs ->
							  mapP (\g -> as !: g) xs
							)
							img
				 ]

-- Vector apply
apply5 :: PAImage Int
apply5 = mapPV
					$: V[(\xs -> mapP (\g -> as !: g)  xs)]
					$: V[img]


-- Vector locally-bound variable
-- Vector lambda
apply6 :: PAImage Int
apply6 = mapPV
					$: Clo {
							 env = (as)
							,scalar = \(as) xs -> V[mapP (\g -> as !: g) xs]
							,lifted = \(ATup1 n as) xs -> L[mapP (\g -> as !: g) xs] n
						 }
					$: img

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

-- final form before inlining
applyV :: PA Int :-> PAImage Int :-> PAImage Int
applyV =
	Clo {
		 env = ()
		,lifted = (...ignored inside context...)
		,scalar =
			\() as -> 
				mapPV
						$: Clo {
								 env = (as)
								,scalar = (...ignored inside mapP...)
								,lifted =
									\(ATup1 n as) xs ->
										replPA n mapPV
											$:L AClo {
													  aenv = (as)
													 ,scalar = (...ignored inside mapP...)
													 ,lifted =
													 		\(ATup1 n as) g ->
																replicatePA n indexPV
																	$:L as
																	$:L g
													}
											$:L xs
							 }
						$: img
	}


