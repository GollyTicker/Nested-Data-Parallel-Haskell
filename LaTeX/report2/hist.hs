hist1 :: PA (PA Int)) :-> PA Int
V[hist] $: img
  = sparseToDensePV
      $: (plusIntV $: gmax $: 1)
      $: 0
      $: mapPV
         $: Clo () _ lambdaGL
         $: groupPV
            $: sortPV
               $: concatPV
                  $: img
