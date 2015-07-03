
"First step of optimization. Optimizing on parallel arrays"
"After this step comes distributed types and stream/communication fusioning."

"$   and .   are application/composition of usual functions"
"$:  and $:L  are scalar and lifted application of vectorized functions"

V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusIntV 0    -- accu
            . sparseToDensePS (plusIntS gmax 1) 0   -- hist end
            . (\g -> (,)L (headPL g) (lengthPL g))  -- ignored argument
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img
        n = length a
        as = replPS (lengthPS img)            -- replicate width
             . floorL                                     -- normalize and scale
               (multDoubleL (int2DoubleL (replPS n gmax)))
             . divL
                 (minusL (int2DoubleL a) (  replPS n (int2Double (headPS a))  ))
             . replPS n
             $ minusDoubleS (int2Double (lastPS a)) a0
    in (\xs -> -- apply on every pixel -- core of nested data parallelism here!
         unconcatPS xs . indexPL (concatPS . replPL (lengths (getSegd as)) as) . concatPS $ xs
       ) img
