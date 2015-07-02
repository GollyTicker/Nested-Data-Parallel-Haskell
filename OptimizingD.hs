
"First step of optimization. Optimizing on parallel arrays" (after that comes distributed types and stream/communication fusioning)

"$   and .   are application/composition of usual functions"
"$:  and .:  are scalar application/composition of vectorized functions"
"$:L and .:L are lifted application/composition of usual functions"


"Inlining more complex definitions"

V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusIntS 0    -- accu
            . sparseToDensePS (plusIntS gmax 1) 0   -- hist end
            . (\g -> ATup2 (headPL g) (lengthPL g))  -- ignored argument
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img
        n = lengthPS a
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

note:
 Der Ausdruck (concatPS . replPL (lengths (getSegd as)) as) sorgt lediglich dafür,
 dass der bereits einmal senkrecht-replizierte AkkumulatorArray nochmal waagerecht-repliziert wird.
 Damit steht jedem Pixel eine direkte Kopie des gesamten Akkumulators zur Verfügung.
 Durch "Work Efficient Vectorization" kann diese Replikation effizienter gemacht werden.


"inline groupPS and join lambda"
groupPS :: PA Int -> PA (PA Int)
groupPS as = AArr as (convert (split 0 as)) -- convert und split laufen distributed


V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusIntS 0    -- accu
            . sparseToDensePS (plusIntS gmax 1) 0   -- hist end
            . (\as ->
                let g = AArr as (convert (split 0 as)) 
                in  ATup2 (headPL g) (lengthPL g)
              )
            . sortPS
            . concatPS                              -- hist begin
            $ img
        n = lengthPS a
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


"inline sparseToDensePS and scanlP"
sparseToDensePS ps =
  joinD
  . cmapD (\c ps' -> sparseToDenseS c size z ps')
  . splitSparseD size
  $ ps
  
scanlPS xs = 
  joinD
  . mapD (\(as,a) -> mapS (f a) as)
  . propagateD f z
  . mapD (scanlS f z)
  . splitD
  $ xs


V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusIntS 0    -- accu
            joinD
            . mapD (\(as,a) -> mapS (f a) as)
            . propagateD plusIntV z
            . mapD (scanlS f z)
            . splitD
            . joinD                 -- hist end
            . cmapD (\c ps' -> sparseToDenseS c (plusIntS gmax 1) 0 ps')
            . splitSparseD (plusIntS gmax 1)
            . (\as ->
                let g = AArr as (convert (split 0 as)) 
                in  ATup2 (headPL g) (lengthPL g)
              )
            . sortPS
            . concatPS                              -- hist begin
            $ img
        n = lengthPS a
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

