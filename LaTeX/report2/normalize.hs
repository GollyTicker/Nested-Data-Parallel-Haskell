V[normalize] :: Int :-> Int :-> PA Int :-> PA Double
V[normalize] $: someA0 $: someAgmax $: someAccu
  = (\a0 ->
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
          $: someAccu
      ) (minusDoubleV $: (fromIntegralV $: someAgmax) $: a0)
    ) (fromIntegralV $: someA0)
