
"Final optimized code"

hbalanceV :: PA (PA Int) -> PA (PA Int)
hbalanceV img =
  let a :: PA Int -- Histogram
      a = joinD                                           -- accu end
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
      n :: Int
      n = lengthPS a                                      -- 2
      
      a0, divisor, gmax' :: Double
      a0      = int2Double . headPS $ a                                         -- 3, variables for normalize and scale
      divisor = minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a
      gmax'   = int2Double $ gmax
      
      normScale :: Int -> Int
      normScale = floorDouble . (flip multDouble) gmax' . (flip divDouble) divisor . (flip minusDouble) a0 . int2Double -- 0, body of normalize and scale
      
      as :: PA Int                                        -- final mapping array
      as = joinD . mapD (mapS normScale) $ a              -- 4, normalize and scale applied
      
      pixelReplicate :: PA Int -> PA (PA (PA Int))
      pixelReplicate = concatPS . replPL (lengths (getSegd xs)) . replPS (lengthPS img)                                 -- 0, artifact of NDP
      
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
