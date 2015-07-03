V[accu] :: PA Int :-> PA Int
V[accu] $: h
  = (\xs -> scanlPV $: plusIntV $: 0 $: xs) $: h
  = scanlPV $: plusIntV $: 0 $: h



