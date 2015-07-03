-- vectorized gmax
V[gmax] :: Int
V[gmax] = 225

V[hbalance] :: PA (PA Int) :-> PA (PA Int)
  = Clo {
       env = ()
      ,scalar = V[hbalanceBody]
      ,lifted = (...omitted here...)
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
  


