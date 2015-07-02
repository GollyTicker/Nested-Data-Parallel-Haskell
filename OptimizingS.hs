"nächster Schritt. Fusioning mit stream/unstream-Funktionen (z.B. streamS)"

{-

propagateD :: Dist (PA a) -> Dist (PA a, a)

sparseToDenseD :: Int -> Int -> Dist (PA (Int,a)) -> Dist (PA a)
splitSparseD :: PA (Int,a) -> Dist (PA (Int,a)

split :: Int -> PA a -> PA (a,Int,Int)        functions implementing the distributed groupP on the segment-descriptor
convert :: PA (a,Int,Int) -> PA (Int,Int)

lengths :: Segd -> PA Int   retreives the lengths of each subarray.

-- local raw-array functions implemented using streams
multS, divDoubleS, ... :: Unboxed a => Vector a -> Vector a -> Vector a

-- primitive arithmetic operations
mult, div, ... :: a -> a -> a

-}


V[hbalance] $: img :: PA (PA Int)
  = let a = joinD
            . mapD (\(as,a) -> mapS (plusInt a) as)
            . propagateD plusInt 0
            . mapD (scanlS plusInt 0)
            . sparseToDenseD (plusInt gmax 1) 0 -- hist end
            . splitSparseD (plusInt gmax 1)
            . (\as -> let g = AArr as (convert (split 0 as))
                      in  ATup2 (headPL g) (lengthPL g)
              )
            . sortPS
            . concatPS                                   -- hist begin
            $ img
        n = lengthPS a
        as = joinD
             . zipWith4D
                ( \a b c d -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS (int2DoubleS c) d) b) a)  )  -- normalize and scale
                ( replD n . int2Double $ gmax )
                ( replD n . minusDouble (int2Double (lastPS a)) . headPS $ a )
                ( replD n . int2Double . headPS $ a )
              . splitD
              $ a
        pixelReplicate = concatPS . replPL (lengths (getSegd xs)) . replPS (lengthPS img)
    in unconcatPS img
       . indexPL (pixelReplicate as)  -- apply on every pixel -- core of nested data parallelism here!
       . concatPS
       $ img


"Definitionen: "
multDoubleS :: Vector Double -> Vector Double -> Vector Double
multDoubleS = \as bs -> unstream . zipWithSt mult (stream as) (stream bs)
  = \as -> unstream . zipWithSt multDouble (stream as) . stream

floorDoubleS :: Vector Double -> Vector Int
floorDoubleS = unstream . mapSt floorDouble . stream


"Verbesserung des Lambdas"

( \a b c d -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS (int2DoubleS c) d) b) a)  )

    "formatieren"

  = ( \a b c d ->
      floorDoubleS
        (multDoubleS
          (divDoubleS
            (minusDoubleS
              (int2DoubleS
                c
              )
              d
            )
            b
          )
          a
        )
    )
    
    "definitionen einsetzten"

  = ( \a b c d ->
      (unstream . mapSt floorDouble . stream)
        ( (\as -> unstream . zipWithSt multDouble (stream as) . stream $ a)
          ( (\as -> unstream . zipWithSt divDouble (stream as) . stream $ b)
            ( (\as -> unstream . zipWithSt minusDouble (stream as) . stream $ d)
              ( (unstream . mapSt int2Double . stream $ c) )
            )
          )
        )
    )
    
    "lambda reductions"
    
  = ( \a b c d ->
      (unstream . mapSt floorDouble . stream)
        (unstream . zipWithSt multDouble (stream (unstream . zipWithSt divDouble (stream (unstream . zipWithSt minusDouble (stream (unstream . mapSt int2Double . stream $ c)) . stream $ d)) . stream $ b)) . stream $ a)
    )
    
    "ready for rules"
    
  = ( \a b c d ->
      (
        unstream
        . mapSt floorDouble
        . stream
        . unstream
        . zipWithSt
            multDouble
            ( stream
              . unstream
              . zipWithSt
                  divDouble
                  ( stream
                    . unstream
                    . zipWithSt
                        minusDouble
                        ( stream
                          . unstream
                          . mapSt int2Double
                          . stream
                          $ c)
                    . stream
                    $ d)
              . stream
              $ b)
        . stream
        $ a)
    )
    
    "apply 4x => unstream . stream = id"
    
  = ( \a b c d ->
      (
        unstream
        . mapSt floorDouble
        . zipWithSt
            multDouble
            (zipWithSt
               divDouble
               (zipWithSt
                   minusDouble
                   (mapSt int2Double . stream $ c)
               . stream
               $ d)
             . stream
             $ b)
        . stream
        $ a)
    )
    
    "rules zipWith4St sowie zipWithSt/mapSt"
    "\as bs cs ds -> zipWithSt f (zipWithSt g (zipWithSt h as bs) cs) ds = zipWith4St ( \a b c d -> f (g (h a b) c) d) as bs cs ds"
    
  = (\a b c d -> unstream
                . mapSt floorDouble
                . zipWith4St
                    ( \c' d' b' a' -> multDouble (divDouble (minusDouble (int2Double c') d') b') a')
                    (stream $ c)
                    (stream $ d)
                    (stream $ b)
                    (stream $ a)
    )
  
   "zurück zum Code!"
   
V[hbalance] $: img :: PA (PA Int)
  = let a = joinD
            . mapD (\(as,a) -> mapS (plusInt a) as)
            . propagateD plusInt 0
            . mapD (scanlS plusInt 0)
            . sparseToDenseD (plusInt gmax 1) 0 -- hist end
            . splitSparseD (plusInt gmax 1)
            . (\as -> let g = AArr as (convert (split 0 as))
                      in  ATup2 (headPL g) (lengthPL g)
              )
            . sortPS
            . concatPS                                   -- hist begin
            $ img
        n = lengthPS a
        as = joinD
             . zipWith4D
                (\a b c d -> unstream
                             . mapSt floorDouble
                             . zipWith4St             -- normalize and scale
                                ( \c' d' b' a' -> multDouble (divDouble (minusDouble (int2Double c') d') b') a')
                                (stream $ c)
                                (stream $ d)
                                (stream $ b)
                                (stream $ a)
                )
                ( replD n . int2Double $ gmax )
                ( replD n . minusDouble (int2Double (lastPS a)) . headPS $ a )
                ( replD n . int2Double . headPS $ a )
              . splitD
              $ a
        pixelReplicate = concatPS . replPL (lengths (getSegd xs)) . replPS (lengthPS img)
    in unconcatPS img
       . indexPL (pixelReplicate as)  -- apply on every pixel -- core of nested data parallelism here!
       . concatPS
       $ img
   = ( \a b c d ->
      (
        unstream
        . mapSt floorDouble
        . zipWith4St
            ( \c' d' b' a' -> multDouble (divDouble (minusDouble (int2Double c') d') b') a')
            (stream $ c)
            (stream $ d)
            (stream $ b)
            (stream $ a)
    )
    

