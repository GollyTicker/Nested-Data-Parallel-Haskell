
"Second step of optimization. Optimizing on parallel distributed arrays" (after that comes stream fusioning)

"$   and .   are application/composition of usual functions"
"$:  and .:  are scalar application/composition of vectorized functions"
"$:L and .:L are lifted application/composition of usual functions"


"Inlining more complex definitions"

V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusInt 0    -- accu
            . sparseToDensePS (plusInt gmax 1) 0   -- hist end
            . (\g -> ATup2 (headPL g) (lengthPL g))  -- ignored argument
            . groupPS
            . sortPS
            . concatPS                              -- hist begin
            $ img
        n = lengthPS a
        as = replPS (lengthPS img)            -- replicate width
             . floorDoubleL                                     -- normalize and scale
             . multDoubleL (int2DoubleL (replPS n gmax))
             . divL
                 (minusL (int2DoubleL a) (  replPS n (int2Double (headPS a))  ))
             . replPS n
             $ minusDouble (int2Double (lastPS a)) (int2Double (headPS a))
    in (\xs -> -- apply on every pixel -- core of nested data parallelism here!
         unconcatPS xs . indexPL (concatPS . replPL (lengths (getSegd xs)) $ as) . concatPS $ xs
       ) img

note:
 Der Ausdruck (concatPS . replPL (lengths (getSegd as)) as) sorgt lediglich dafür,
 dass der bereits einmal senkrecht-replizierte AkkumulatorArray nochmal waagerecht-repliziert wird.
 Damit steht jedem Pixel eine direkte Kopie des gesamten Akkumulators zur Verfügung.
 Durch "Work Efficient Vectorization" kann diese Replikation effizienter gemacht werden.


VERWORFEN: "inline groupPS and join lambda"
  groupPS :: PA Int -> PA (PA Int)
  groupPS as = AArr as (convert (segdSplitMerge 0 as)) -- convert und segdSplitMerge laufen distributed. "segdSplitMerge" heißt in groupP.hs "split"

STATTDESSEN: "Folgende Otimierung die sich die unnöötige Kommunikation dazwischen spart."

f xs =                                   -- (Value,StartIdx,Count)     (Value,Count)
  let tripletToATup2 :: Dist (LinkedList (Int,Int,Int)) -> Dist (PA (Int,Int))
      tripletToATup2 ts = DATup2 (toPA . mapLL fst) (toPA . mapLL thr) -- mapLL is for mapping over the local linked lists chunks
  in joinD . mapD localSegdToTup2 . segdSplitMerge 0


V[hbalance] $: img :: PA (PA Int)
  = let a = scanlPS plusInt 0    -- accu
            . sparseToDensePS (plusInt gmax 1) 0   -- hist end
            . joinD
            . mapD localSegdToTup2
            . segdSplitMerge 0
            . sortPS
            . concatPS                              -- hist begin
            $ img
        n = lengthPS a
        as = replPS (lengthPS img)            -- replicate width
             . floorDoubleL                                     -- normalize and scale
             . multDoubleL (int2DoubleL (replPS n gmax))
             . divL
                 (minusL (int2DoubleL a) (  replPS n (int2Double (headPS a))  ))
             . replPS n
             $ minusDouble (int2Double (lastPS a)) (int2Double (headPS a))
    in (\xs -> -- apply on every pixel -- core of nested data parallelism here!
         unconcatPS xs . indexPL (concatPS . replPL (lengths (getSegd xs)) $ as) . concatPS $ xs
       ) img


"inline sparseToDensePS and scanlP and concatPS"
sparseToDensePS ps =
  joinD
  . sparseToDenseD size z
  . splitSparseD size
  $ ps
  
scanlPS xs = 
  joinD
  . mapD (\(as,a) -> mapS (f a) as)
  . propagateD f z
  . mapD (scanlS f z)
  . splitD
  $ xs

-- no need to inline: concatPS xs = joinD . cmapD (\ctx (DAArr from to n segd chunk) -> DAInt from to n chunk) . splitD

"Explicit calls to AArr and ATup2 and such mean global communication"
"Types of PAs are global. Types of Dist are local."


V[hbalance] $: img :: PA (PA Int)
  = let a = joinD
            . mapD (\(as,a) -> mapS (plusInt a) as)
            . propagateD plusInt 0                 -- propagateD :: Dist (PA a) -> Dist (PA a, a)
            . mapD (scanlS plusInt 0)
            . splitD                    -- split join rule fires
            . joinD                 -- hist end
            . sparseToDenseD (plusInt gmax 1) 0
            . splitSparseD (plusInt gmax 1)      -- splitSparseD :: PA (Int,a) -> Dist (PA (Int,a)
            . joinD
            . mapD localSegdToTup2
            . segdSplitMerge 0
            . sortPS
            . concatPS                                   -- hist begin
            $ img
        n = lengthPS a
        as = replPS (lengthPS img)            -- replicate width
             . floorDoubleL                                     -- normalize and scale
             . multDoubleL (int2DoubleL ( replPS n gmax ))
             . divL
                 (minusL (int2DoubleL a) ( replPS n (int2Double (headPS a)) ))
             . replPS n
             $ minusDouble (int2Double (lastPS a)) (int2Double (headPS a))
    in (\xs -> -- apply on every pixel -- core of nested data parallelism here!
         unconcatPS xs . indexPL (concatPS . replPL (lengths (getSegd xs)) $ as) . concatPS $ xs
       ) img

"optimise and rule firings"

"rule: " splitD . joinD = id

let a =
  joinD
  . mapD (\(as,a) -> mapS (plusInt a) as)
  . propagateD plusInt 0                 -- propagateD :: Dist (PA a) -> Dist (PA a, a)
  . mapD (scanlS plusInt 0)
  . sparseToDenseD (plusInt gmax 1) 0 -- hist end
  . splitSparseD (plusInt gmax 1)      -- splitSparseD :: PA (Int,a) -> Dist (PA (Int,a)
  . joinD
  . mapD localSegdToTup2
  . segdSplitMerge 0
  . sortPS
  . concatPS                                   -- hist begin
  $ img

VERWORFEN: "rule: " mapD f . cmapD g = cmapD (\ctx x -> f (g ctx x))
-- any further inlining and optimization doesn't reveal anything interesting

V[hbalance] $: img :: PA (PA Int)
  = let a = joinD
            . mapD (\(as,a) -> mapS (plusInt a) as)
            . propagateD plusInt 0                 -- propagateD :: Dist (PA a) -> Dist (PA a, a)
            . mapD (scanlS plusInt 0)
            . sparseToDenseD (plusInt gmax 1) 0 -- hist end
            . splitSparseD (plusInt gmax 1)        -- splitSparseD :: PA (Int,a) -> Dist (PA (Int,a)
            . joinD
            . mapD localSegdToTup2
            . segdSplitMerge 0
            . sortPS
            . concatPS                                   -- hist begin
            $ img
        n = lengthPS a
        as = replPS (lengthPS img)            -- replicate width
             . floorDoubleL                                     -- normalize and scale
             . multDoubleL (int2DoubleL ( replPS n gmax ))
             . divL
                 (minusL (int2DoubleL a) ( replPS n (int2Double (headPS a)) ))
             . replPS n
             $ minusDouble (int2Double (lastPS a)) (int2Double (headPS a))
    in (\xs -> -- apply on every pixel -- core of nested data parallelism here!
         unconcatPS xs
         . indexPL (concatPS . replPL (lengths (getSegd xs)) as)
         . concatPS
         $ xs
       ) img



"inline primitive lifted operations"
floorDoubleL = joinD . mapD floorDoubleS . splitD
multDoubleL
  = \as bs -> joinD ( zipWithD multDoubleS (splitD as) (splitD bs) )
  = \as -> joinD . zipWithD multDoubleS (splitD as) . splitD


let as = replPS (lengthPS img)            -- replicate width
         . (joinD . mapD floorDoubleS . splitD)                                     -- normalize and scale
         . (\as -> joinD . zipWithD multDoubleS (splitD as) . splitD) ((joinD . mapD int2DoubleS . splitD) ( replPS n gmax ))
         . (\as -> joinD . zipWithD divDoubleS (splitD as) . splitD)
             ((\as -> joinD . zipWithD minusDoubleS (splitD as) . splitD) ((joinD . mapD int2DoubleS . splitD) a) ( replPS n (int2Double (headPS a)) ))
         . replPS n
         $ minusDouble (int2Double (lastPS a)) (int2Double (headPS a))
         
"reformat"

let as = replPS (lengthPS img)            -- replicate width
         . joinD
         . mapD floorDoubleS
         . splitD                                   -- normalize and scale
         . (\as -> joinD . zipWithD multDoubleS (splitD as) . splitD) (joinD . mapD int2DoubleS . splitD .  replPS n $ gmax )
         . (\as -> joinD . zipWithD divDoubleS (splitD as) . splitD)
             ((\as -> joinD . zipWithD minusDoubleS (splitD as) . splitD) ((joinD . mapD int2DoubleS . splitD) a) ( replPS n (int2Double (headPS a)) ))
         . replPS n
         $ minusDouble (int2Double (lastPS a)) (int2Double (headPS a))


"inserting lamdas"

let as = replPS (lengthPS img)            -- replicate width
         . joinD
         . mapD floorDoubleS
         . splitD                                     -- normalize and scale
         . joinD
         . zipWithD multDoubleS (splitD . joinD . mapD int2DoubleS . splitD .  replPS n $ gmax)
         . splitD
         . joinD
         . zipWithD
            divDoubleS
            ( splitD
              . joinD
              . zipWithD minusDoubleS (splitD . joinD . mapD int2DoubleS . splitD $ a)
              . splitD
              . replPS n
              . int2Double
              . headPS
              $ a
            )
         . splitD
         . replPS n
         $ minusDouble (int2Double (lastPS a)) (int2Double (headPS a))

"fire splitD/joinD rule 5 times"

let as = replPS (lengthPS img)            -- replicate width
         . joinD
         . mapD floorDoubleS              -- normalize and scale
         . zipWithD multDoubleS (mapD int2DoubleS . splitD . replPS n $ gmax)
         . zipWithD
            divDoubleS
            ( zipWithD minusDoubleS (mapD int2DoubleS . splitD $ a)
              . splitD
              . replPS n
              . int2Double
              . headPS
              $ a
            )
         . splitD
         . replPS n
         $ minusDouble (int2Double (lastPS a)) (int2Double (headPS a))
         
"fire mapD/zipWithD" mapD f . zipWithD g as = zipWithD (\x y -> f (g x y)) as
"fire flip/zipWithD" zipWith f as bs = zipWith (flip f) bs as
  "on the zipWith divDoubleS"

let as = replPS (lengthPS img)            -- replicate width
         . joinD
         . zipWithD
            (\x y -> floorDoubleS (multDoubleS x y))
            (mapD int2DoubleS . splitD . replPS n $ gmax) -- normalize and scale
         . zipWithD
            (flip divDoubleS)
            ( splitD
             . replPS n
             . minusDouble (int2Double (lastPS a))
             . int2Double
             . headPS
             $ a
            )
         . zipWithD
            minusDoubleS
            (mapD int2DoubleS . splitD $ a)
         . splitD
         . replPS n
         . int2Double
         . headPS
         $ a

"fire zipWithD/zipWithD/zipWithD" zipWithD f as . zipWith g bs . zipWithD h cs = zipWith4D (\a b c d -> f a (g b (h c d)) as bs cs
where
  ( \a b c d -> (\x y -> floorDoubleS (multDoubleS x y)) a ((flip divDoubleS) b (minusDoubleS c d)) )
  further simplifies to
  ( \a b c d -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS c d) b) a)  )


let as = replPS (lengthPS img)            -- replicate width
         . joinD
         . zipWith4D
            ( \a b c d -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS c d) b) a)  )
            (mapD int2DoubleS . splitD . replPS n $ gmax) -- normalize and scale
            ( splitD
             . replPS n
             . minusDouble (int2Double (lastPS a))
             . int2Double
             . headPS
             $ a
            )
            (mapD int2DoubleS . splitD $ a)
         . splitD
         . replPS n
         . int2Double
         . headPS
         $ a

"replikation lokal durchführen"
"splitD/replPS[Int]" für Int -> Int -> PA Int
  splitD . replPS n = replD n

replD :: Int -> Int -> Dist (PA Int)
replD n x =
  cgenerateD (\ctx ->
                let (from, to) = arrayRange ctx n -- calculate which range we are responsible for
                in  DAInt from to n (replByteArray (from - to) x)
  )

let as = replPS (lengthPS img)            -- replicate width
         . joinD
         . zipWith4D
            ( \a b c d -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS c d) b) a)  )
            (mapD int2DoubleS . replD n $ gmax) -- normalize and scale
            ( replD n
              . minusDouble (int2Double (lastPS a))
              . int2Double
              . headPS
              $ a
            )
            (mapD int2DoubleS . splitD $ a)
         . replD n
         . int2Double
         . headPS
         $ a

"zipWithD/mapD" -- auf unteres anwenden
  zipWithD f (mapD g as) = zipWith (\a -> f (g a))

"mapD/replD" -- auf oberes andwenden
  mapD f . replD n = replD n . f
  
let as = replPS (lengthPS img)            -- replicate width
         . joinD
         . zipWith4D
            ( \a b c d -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS (int2DoubleS c) d) b) a)  )
            (replD n $ int2Double gmax) -- normalize and scale
            ( replD n
              . minusDouble (int2Double (lastPS a))
              . int2Double
              . headPS
              $ a
            )
            (splitD a)
         . replD n
         . int2Double
         . headPS
         $ a

"drittes und viertes Argument im zipWith4D flippen"

let as = replPS (lengthPS img)            -- replicate width
         . joinD
         . zipWith4D
            ( \a b d c -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS (int2DoubleS c) d) b) a)  )  -- normalize and scale
            ( replD n . int2Double $ gmax )
            ( replD n . minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a )
            ( replD n . int2Double . headPS $ a )
          . splitD
          $ a

V[hbalance] $: img :: PA (PA Int)
  = let a = joinD
            . mapD (\(as,a) -> mapS (plusInt a) as)
            . propagateD plusInt 0                 -- propagateD :: Dist (PA a) -> Dist (PA a, a)
            . mapD (scanlS plusInt 0)
            . sparseToDenseD (plusInt gmax 1) 0 -- hist end
            . splitSparseD (plusInt gmax 1)        -- splitSparseD :: PA (Int,a) -> Dist (PA (Int,a)
            . joinD
            . mapD localSegdToTup2
            . segdSplitMerge 0
            . sortPS
            . concatPS                                   -- hist begin
            $ img
        n = lengthPS a
        as = replPS (lengthPS img)            -- replicate width
             . joinD
             . zipWith4D
                ( \a b d c -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS (int2DoubleS c) d) b) a)  )  -- normalize and scale
                ( replD n . int2Double $ gmax )
                ( replD n . minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a )
                ( replD n . int2Double . headPS $ a )
              . splitD
              $ a
    in (\xs -> -- apply on every pixel -- core of nested data parallelism here!
         unconcatPS xs
         . indexPL (concatPS . replPL (lengths (getSegd xs)) $ as)
         . concatPS
         $ xs
       ) img

"zipWith4D/replD/splitD" =>
  zipWith4D f (replD n a) (replD n b) (replD n c) . splitD = mapD (f a b c)


V[hbalance] $: img :: PA (PA Int)
  = let a = joinD
            . mapD (\(as,a) -> mapS (plusInt a) as)
            . propagateD plusInt 0                 -- propagateD :: Dist (PA a) -> Dist (PA a, a)
            . mapD (scanlS plusInt 0)
            . sparseToDenseD (plusInt gmax 1) 0 -- hist end
            . splitSparseD (plusInt gmax 1)        -- splitSparseD :: PA (Int,a) -> Dist (PA (Int,a)
            . joinD
            . mapD localSegdToTup2
            . segdSplitMerge 0
            . sortPS
            . concatPS                                   -- hist begin
            $ img
        n = lengthPS a
        as = replPS (lengthPS img)            -- replicate width
             . joinD
             . mapD  -- normalize and scale
                ( \a b d c -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS (int2DoubleS c) d) b) a)  )
                    ( replD n . int2Double $ gmax )
                    ( replD n . minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a )
                    ( replD n . int2Double . headPS $ a )
              . splitD
             $ a
    in (\xs -> -- apply on every pixel -- core of nested data parallelism here!
         unconcatPS xs
         . indexPL (concatPS . replPL (lengths (getSegd xs)) $ as)
         . concatPS
         $ xs
       ) img

"ein bisschen saubern"
  "senkrechtes replizieren inlined"
  "replizieren zu jedem Pixel in eine eigene Funktion gepackt. zum lesweren leseverständnis"
  "lambda (\xs -> ) img agewandt"

V[hbalance] $: img :: PA (PA Int)
  = let a = joinD
            . mapD (\(as,a) -> mapS (plusInt a) as)
            . propagateD plusInt 0
            . mapD (scanlS plusInt 0)
            . sparseToDenseD (plusInt gmax 1) 0 -- hist end
            . splitSparseD (plusInt gmax 1)
            . joinD
            . mapD localSegdToTup2
            . segdSplitMerge 0
            . sortPS
            . concatPS                                   -- hist begin
            $ img
        n = lengthPS a
        as = joinD
             . mapD  -- normalize and scale
                ( \a b d c -> floorDoubleS (multDoubleS (divDoubleS (minusDoubleS (int2DoubleS c) d) b) a)  )
                    ( replD n . int2Double $ gmax )
                    ( replD n . minusDouble (int2Double (lastPS a)) . int2Double . headPS $ a )
                    ( replD n . int2Double . headPS $ a )
              . splitD
             $ a
        pixelReplicate = concatPS . replPL (lengths (getSegd xs)) . replPS (lengthPS img)
    in unconcatPS img
       . indexPL (pixelReplicate as)  -- apply on every pixel -- core of nested data parallelism here!
       . concatPS
       $ img

{-

propagateD :: Dist (PA a) -> Dist (PA a, a)

sparseToDenseD :: Int -> Int -> Dist (PA (Int,a)) -> Dist (PA a)
splitSparseD :: PA (Int,a) -> Dist (PA (Int,a)
tripletToATup2 :: (LinkedList (Int,Int,Int)) -> (PA (Int,Int))
segdSplitMerge :: Int -> PA Int -> Dist (LinkedList (Int,Int,Int))


split :: Int -> PA a -> PA (a,Int,Int)        functions implementing the distributed groupP on the segment-descriptor
convert :: PA (a,Int,Int) -> PA (Int,Int)

lengths :: Segd -> PA Int   retreives the lengths of each subarray.

-- local raw-array functions implemented using streams
multS, divDoubleS, ... :: Unboxed a => Vector a -> Vector a -> Vector a

-- primitive arithmetic operations
mult, div, ... :: a -> a -> a

replD :: Int -> a -> Dist (PA a)

-}

"nächster Schritt. Fusioning mit stream/unstream-Funktionen (z.B. streamS)"
