V[scale] :: Int :-> PA Double :-> PA Int
V[scale] $: someInt $: someNormHist
  = mapPV
      $: Clo {
           env = (someInt)
          ,lifted = \(ATup1 n gmax) a -> replPA n floorV $:L (replPA n multDoubleV $:L (replPA fromIntegralV n $:L gmax) $:L a)
          ,scalar = (...ignored inside mapP...)
         }
      $: someNormHist
