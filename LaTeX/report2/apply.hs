V[apply] :: PA Int :-> PAImage Int :-> PAImage Int
V[apply] $: someAkku $: someImage
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


