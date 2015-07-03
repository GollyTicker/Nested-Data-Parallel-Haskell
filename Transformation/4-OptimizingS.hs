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

replD :: Int -> a -> Dist (PA a)

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
             . mapD  -- normalize and scale
                ( \a b c d -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS (int2DoubleS d) c) b) a)  )
                    ( replD n . int2Double $ gmax )       -- gmax
                    ( replD n . minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a )    -- divisor
                    ( replD n . int2Double . headPS $ a )    -- a0
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
                d
              )
              c
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
            ( (\as -> unstream . zipWithSt minusDouble (stream as) . stream $ c)
              ( (unstream . mapSt int2Double . stream $ d) )
            )
          )
        )
    )
    
    "lambda reductions"
    
  = ( \a b c d ->
      (unstream . mapSt floorDouble . stream)
        (unstream . zipWithSt multDouble (stream (unstream . zipWithSt divDouble (stream (unstream . zipWithSt minusDouble (stream (unstream . mapSt int2Double . stream $ d)) . stream $ c)) . stream $ b)) . stream $ a)
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
                          $ d)
                    . stream
                    $ c)
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
                   (mapSt int2Double . stream $ d)
               . stream
               $ c)
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
                    ( \d' c' b' a' -> multDouble (divDouble (minusDouble (int2Double d') c') b') a')
                    (stream $ d)
                    (stream $ c)
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
             . mapD  -- normalize and scale
                (\a b c d -> unstream
                             . mapSt floorDouble
                             . zipWith4St             -- normalize and scale
                                ( \d' c' b' a' -> multDouble (divDouble (minusDouble (int2Double d') c') b') a')
                                (stream $ d)
                                (stream $ c)
                                (stream $ b)
                                (stream $ a)
                ) ( replD n . int2Double $ gmax )
                  ( replD n . minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a )
                  ( replD n . int2Double . headPS $ a )
              . splitD
             $ a
        pixelReplicate = concatPS . replPL (lengths (getSegd xs)) . replPS (lengthPS img)
    in unconcatPS img
       . indexPL (pixelReplicate as)  -- apply on every pixel -- core of nested data parallelism here!
       . concatPS
       $ img

"jetzt können wir lambda-reduction anwenden! :D yeah!"

        as = joinD
             . mapD  -- normalize and scale
                (\d -> unstream
                       . mapSt floorDouble
                       . zipWith4St             -- normalize and scale
                          ( \d' c' b' a' -> multDouble (divDouble (minusDouble (int2Double d') c') b') a')
                          (stream $ d)
                          (stream . replD n . int2Double . headPS $ a)
                          (stream . replD n . minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a)
                          (stream . replD n . int2Double $ gmax)
                )
             . splitD
             $ a
             
        "zipWith argument flipping. stream $ d wird zum Ende gebracht und verschwindet durch currying"
        
        as = joinD
             . mapD  -- normalize and scale
                (unstream
                 . mapSt floorDouble
                 . zipWith4St             -- normalize and scale
                    ( \c' b' a' d' -> multDouble (divDouble (minusDouble (int2Double d') c') b') a')
                    (stream . replD n . int2Double . headPS $ a)
                    (stream . replD n . minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a)
                    (stream . replD n . int2Double $ gmax)
                 . stream
                )
             . splitD
             $ a
        
        "verallgemeinerung vom folgenden rewrite rule zum float-in einer Konstante in ein zipWithSt wird angewendet"
          zipWithSt f (stream (replD n a)) bs = mapSt (f a) bs
          
        as = joinD
             . mapD  -- normalize and scale
                (unstream
                 . mapSt floorDouble
                 . mapSt
                      ( \c' b' a' d' -> multDouble (divDouble (minusDouble (int2Double d') c') b') a') -- normalize and scale
                        (int2Double . headPS $ a)
                        (minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a)
                        (int2Double $ gmax)
                 . stream
                )
             . splitD
             $ a
             
         "mapSt/mapSt rule fires, extract lambda args into a let, flip arguments into point-free-style"
         
          -- flip :: (a -> b -> c) -> (b -> a -> c)
         
          a0 = int2Double . headPS $ a
          divisor = minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a
          func' = flip func
          gmax' = int2Double $ gmax
          as = joinD
               . mapD
                  (unstream
                   . mapSt ( floorDouble . (flip multDouble) gmax' . (flip divDouble) divisor . (flip minusDouble) a0 . int2Double ) -- normalize and scale
                   . stream
                  )
               . splitD
               $ a
         
         "zurück zum Code"


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
        a0      = int2Double . headPS $ a
        divisor = minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a
        gmax'   = int2Double $ gmax
        as = joinD
             . mapD
                (unstream
                 . mapSt ( floorDouble . (flip multDouble) gmax' . (flip divDouble) divisor . (flip minusDouble) a0 . int2Double ) -- normalize and scale
                 . stream
                )
             . splitD
             $ a
        pixelReplicate = concatPS . replPL (lengths (getSegd xs)) . replPS (lengthPS img)
    in unconcatPS img
       . indexPL (pixelReplicate as)  -- apply on every pixel -- core of nested data parallelism here!
       . concatPS
       $ img

      "zurückwandeln der stream-functions (unstream . mapSt f . stream -> mapS f )"
      "und säubern"

V[hbalance] $: img :: PA (PA Int)
  = let a = joinD                                           -- accu end
            . mapD (\(as,a) -> mapS (plusInt a) as)
            . propagateD plusInt 0
            . mapD (scanlS plusInt 0)                       -- accu begin
            . sparseToDenseD (plusInt gmax 1) 0             -- hist end
            . splitSparseD (plusInt gmax 1)
            . (\as -> let g = AArr as (convert (split 0 as))
                      in  ATup2 (headPL g) (lengthPL g)
              )
            . sortPS
            . concatPS                                      -- hist begin
            $ img                                           -- 1
        
        n = lengthPS a                                      -- 2
        
        a0      = int2Double . headPS $ a                                         -- 3, variables for normalize and scale
        divisor = minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a
        gmax'   = int2Double $ gmax
        normScale = floorDouble . (flip multDouble) gmax' . (flip divDouble) divisor . (flip minusDouble) a0 . int2Double -- 0, body of normalize and scale
        
        as = joinD . mapD (mapS normScale) . splitD $ a              -- 4, normalize and scale applied
        
        pixelReplicate = concatPS . replPL (lengths (getSegd xs)) . replPS (lengthPS img)                                 -- 0
        
    in unconcatPS img
       . indexPL (pixelReplicate as)  -- 5, apply. core of nested data parallelism here!
       . concatPS
       $ img


{-

joinD             :: forall a.    Dist a -> a
splitD            :: forall a.    a -> Dist a
mapD              :: forall a b.  (a -> b) -> Dist a -> Dist b
propagateD        :: forall a.    Dist (PA a) -> Dist (PA a, a)
replD             :: forall a.    Int -> a -> Dist (PA a)

sparseToDenseD    :: forall a.    Int -> Int -> Dist (PA (Int,a)) -> Dist (PA a)
splitSparseD      :: forall a.    PA (Int,a) -> Dist (PA (Int,a)



split             :: forall a.    Int -> PA a -> PA (a,Int,Int)         functions implementing the distributed groupP on the segment-descriptor
convert           :: forall a.    PA (a,Int,Int) -> PA (Int,Int)

lengths           ::              Segd -> PA Int                        retreives the lengths of each subarray.

scanlS            :: forall a.    (a -> a -> a) -> a -> Vector a -> Vector a
mapS              :: forall a b.  (a -> b) -> Vector a -> Vector b

multDoubleS,
divDoubleS, ...   ::              Vector Double -> Vector Double -> Vector Double      local raw-array functions

multDouble,
divDouble, ...    ::              Double -> Double -> Double                           primitive arithmetic operations
int2Double        ::              Int -> Int

flip              :: forall a b c.    (a -> b -> c) -> (b -> a -> c)


headPS,lastPS     :: forall a.    PA a -> a                             global array operations
lengthPS          :: forall a.    PA a -> Int
concatPS          :: forall a.    PA (PA a) -> PA a
unconcatPS        :: forall a.    PA (PA a) -> PA a -> PA (PA a)

headPL            :: forall a.    PA (PA a) -> PA a
lengthPL          :: forall a.    PA (PA a) -> PA Int

replPS            :: forall a.                  Int -> a -> PA a        
replPS            :: forall a => PA Int.        Int -> PA Int -> PA (PA Int)
replPL            :: forall a.                  PA Int -> PA a -> PA (PA a)
replPL            :: forall a => PA (PA Int).   PA Int -> PA (PA (PA Int)) -> PA (PA (PA (PA Int)))

sortPS            :: forall a => Int.           PA Int -> PA Int


-}

