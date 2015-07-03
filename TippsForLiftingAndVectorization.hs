
Naming conventions.

mapP
  is the parallel array variant of map.
  it operates on user-arrays [: a :]

lengthPA
  is the parallel array variant of lengthP which
  operates on the type-dependent representation

mapPV is the vectorized mapP function

mapPS is the concrete scalar version in mapPV
mapPL is the concrete lifted version in mapPV

Let expresion vectorization:
V[ let a = b in c ] = (\a -> V[c]) V[b]
  Proof see #11

Let expression lifting:
L[ let a = b in c ] n
L[ (\a -> c) b ] n
  = L[\a -> c] $:L L[b]
  = AClo {
       aenv = ATupk n y1..yk
      ,ascalar = \(y1..yk) a -> V[c]
      ,alifted = \(ATupk n y1..yk) a -> L[c] n
    } $:L L[b] n
  = (\a -> (L[c] n)) (L[b] n)

V[ f ] = V[ \a -> b ] where f is declared top-level
V[ f ] =
  Clo {
     env = ()
    ,scalar = \() a -> V[b]
    ,lifted = \(ATup0 n) a -> L[b] n
  }

Tuple Lifting: (,)L -> ATup2

Innerhalb eines mapP wird die scalare Variante nicht mehr benötigt.
mapP f xs wird immer fL verwenden (siehe Core of flattening). 


Optimization steps in GHC: http://stackoverflow.com/questions/12653787/what-optimizations-can-ghc-be-expected-to-perform-reliably/12661937#12661937

* Strictness Alanysis / Immediate calculation
* Common Subexpression elimination
* Inlining
* Merged case expressions
* Communication Fusioning / Stream Fusioning

DESUGARING -> VECTORIZATION -> FUSION -> GANG PARALLELISM

Example Definition: (extracted from DPH Status report)
sumPS = sumD . mapD sumS . splitD


"For an explanation of the used closure conversion: See Higher Order Flattening, p.43, 4.3.2, p.57 in PDF"

"Core of flattening - Vectorized mapP: [Harness2008 Page 20-21]"

"See Higher Order Flattening, p.55, 4.5, p.69 in PDF"

" Definition of mapPV (from lifted combinators and DPH - Status Report)"
mapPV   :: (a :-> b) :-> PA a :-> PA b
mapPV   = Clo () mapP1 mapP2

mapP1 :: () -> (a :-> b) -> PA a :-> PA b
mapP1 _ fv = Clo fv mapPS mapPL

mapP2 :: PA () -> PA (a :-> b) -> PA (PA a :-> PA b)
mapP2 _ fvs = AClo fvs mapPS mapPL

mapPS :: (a :-> b) -> PA a -> PA b
mapPS fv@(Clo env _ fl) as
  = replPS (lengthPS as) fv $:^ as -- which simplifies. (proof see #10)
  = fl (replPS (lengthPS as) env) as

mapPL :: PA (a :-> b) -> PA (PA a) -> PA (PA b)
mapPL
  = \fv ass -> unconcatPA ass $ replsPS (getSegd ass) fv $:L concatPS ass
  = \(AClo aenv _ fl) ass -> unconcatPS ass . fl (replsPS (getSegd ass) aenv) . concatPS $ ass -- simplified #12

"         Definition of zipWithPV       "


zipWithPV :: (a :-> b :-> c) :-> PA a :-> PA b :-> PA c
zipWithPV = closure3' zipWithPP_v zipWithPP_l

zipWithPS :: (a :-> b) -> 
zipWithPS f as bs
        = replPS (lengthPS as) f $:^ as $:^ bs

zipWithPL fs ass bss
        =   unconcatPS ass
        $   replsPS (getSegd ass) fs
        $:^ concatPS ass
        $:^ concatPS bss

"Why scalar and lifted variant in (Array) Closures?"

Closure ---> replP, mapP ---> Array Closure
  ^                               |
  |                               | 
  +----------   indexP    <-------+

In code appliations need to be able to switch inbetween
both varients to account for repliations, mapping and indexing in context.

type Segd = PA (Int,Int)
-- e.g. ATup2 indices lengths
replsPS :: Segd -> PA a -> PA a
replsPS (ATup2 is ls) as
  = concatP [: replP len a | len <- ls, a <- as :]    -- vanilla haskell, vectorize
  = concatPS (zipWithPS replPV ls as)           -- inline zipWithPS
  = concatPS $ replPS (lengthPS as) replPV $:^ ls $:^ as  -- replicate for functions
  = concatPS $ (AClo (replPS (lengthPS as) replPLenv) replPS replPL) $:^ ls $:^ as    -- the curried args in replPLenv is simply a unit
  = concatPS $ (AClo (replPS (lengthPS as) ()) replPS replPL) $:^ ls $:^ as   -- inline replication of units, inline $:L 2x
  = concatPS $ replPL ls as   -- kann weiter durch inlining optimiert werden, da nur die Daten genommen werden.

replPL :: PA Int -> PA a -> PA (PA a)
replPL = ... "replPL [3,0,2] [a,b,c] = [[a,a,a],[],[c,c]] => stored as => AArr [a,a,a,c,c] [(0,3),(3,0),(3,2)]" 
" -> kann effizient implementiert werden"


"Faces: you start seeing faces when you sit too much on your thesis"

Smiley    Description     Dir.Of.Downw.
:->   sharp smile         ->

$:^   eyebrows            ->

:]
[:    rectangular smile   <->

xs)   x-smile             ->

$:a)  eyebrows and smile  ->

$:    uh-oh...            <-


#10: 

mapPS :: (a :-> b) - PA a -> PA b
  = \fv as -> replPa (lengthPA as) fv $:L as
  = \fv@(Clo env fs fl) as -> replPa (lengthPA as) fv $:L as
-- Definition of replPA for Closures
  = AClo (replPA (lengthPA as) env) fs fl $:L as
-- Definition of $:L
  = fl (replPA (lengthPA as) env) as

#12:

mapPL
  = \fv ass -> unconcatPA ass $ replsPA (getSegd ass) fv $:L concatPA ass
  = \fv@(AClo aenv fs fl) ass -> unconcatPA ass $ replsPA (getSegd ass) fv $:L concatPA ass
-- Definition of replsPA for PA (a :-> b)         See "Lifted.Closure" https://hackage.haskell.org/package/dph-lifted-vseg-0.7.0.1/docs/src/Data-Array-Parallel-Lifted-Closure.html#%3A-%3E
  = \(AClo aenv fs fl) ass -> unconcatPA ass $ AClo (replsPA (getSegd ass) aenv) fs fl) $:L concatPA ass
-- Definition of $:L
  = \(AClo aenv fs fl) ass -> unconcatPA ass . fl (replsPA (getSegd ass) aenv) . concatPA $ ass

" Lifted.Combinators: https://hackage.haskell.org/package/dph-lifted-vseg-0.7.0.1/docs/src/Data-Array-Parallel-Lifted-Combinators.html#emptyPP"



                      "REWRITE RULES"

"unitEnv" forall body xs.
            (\(ATup0 n) xs' -> body )
              (replPA (lengthPA xs) ())
              xs
            = (\n xs' -> body ) (lengthPA xs) xs

-- module Data.Array.Parallel.Prim
"replicatePA_Int#" forall n# i#.
  replicatePA_Int# n# i# = U.replicate (I# n#) (I# i#)

-- module Data.Array.Parallel.Unlifted.Distributed.Arrays
"splitD[unbalanced]/joinD" forall g b da.
  splitD g unbalanced (joinD g b da) = da

"splitD[balanced]/joinD" forall g da.
  splitD g balanced (joinD g balanced da) = da

"splitD/splitJoinD" forall g b f xs.
  splitD g b (splitJoinD g f xs) = f (splitD g b xs)

"splitJoinD/joinD" forall g b f da.
  splitJoinD g f (joinD g b da) = joinD g b (f da)

"splitJoinD/splitJoinD" forall g f1 f2 xs.
  splitJoinD g f1 (splitJoinD g f2 xs) = splitJoinD g (f1 . f2) xs

"Seq.zip/joinD[1]" forall g xs ys.
  Seq.zip (joinD g balanced xs) ys
    = joinD g balanced (zipWithD WZip g Seq.zip xs (splitD g balanced ys))

"Seq.zip/joinD[2]" forall g xs ys.
  Seq.zip xs (joinD g balanced ys)
    = joinD g balanced (zipWithD WZip g Seq.zip (splitD g balanced xs) ys)

"Seq.zip/splitJoinD" 
  forall what1 what2 gang f g xs ys
  . Seq.zip (splitJoinD gang (imapD what1 gang f) xs) 
            (splitJoinD gang (imapD what2 gang g) ys)
  = splitJoinD gang 
        (imapD (WFZipMap what1 what2)
               gang (\i zs -> let (as,bs) = Seq.unzip zs
                              in Seq.zip (f i as) (g i bs)))
                    (Seq.zip xs ys)


-- module Data.Array.Parallel.Unlifted.Distributed.Combinators

"imapD/generateD"
  forall wMap wGen gang f g
  . imapD wMap gang f (generateD wGen gang g) 
  = generateD (W.WFMapGen wMap wGen) gang (\i -> f i (g i))

"imapD/generateD_cheap" 
  forall wMap wGen gang f g
  . imapD wMap gang f (generateD_cheap wGen gang g) 
  = generateD (W.WFMapGen wMap wGen) gang (\i -> f i (g i))

"imapD/imapD" 
  forall wMap1 wMap2 gang f g d
  . imapD wMap1 gang f (imapD wMap2 gang g d) 
  = imapD (W.WFMapMap wMap1 wMap2) gang (\i x -> f i (g i x)) d

"zipD/imapD[1]" 
  forall gang f xs ys what
  . zipD (imapD what gang f xs) ys
  = imapD what gang (\i (x,y) -> (f i x, y)) (zipD xs ys)

"zipD/imapD[2]" 
  forall gang f xs ys what
  . zipD xs (imapD what gang f ys)
  = imapD what gang (\i (x,y) -> (x, f i y)) (zipD xs ys)

"zipD/generateD[1]" 
  forall gang f xs what
  . zipD (generateD what gang f) xs
  = imapD what gang (\i x -> (f i, x)) xs

"zipD/generateD[2]" 
  forall gang f xs what
  . zipD xs (generateD what gang f)
  = imapD what gang (\i x -> (x, f i)) xs

-- module Data.Array.Parallel.Unlifted.Distributed.Data.USegd.Split
"splitSD/splitJoinD" 
  forall g d f xs
  . splitSD g d (splitJoinD g f xs)
  = f (splitSD g d xs)

"splitSD/Seq.zip" 
  forall g d xs ys
  . splitSD g d (Seq.zip xs ys) 
  = zipWithD WZip g Seq.zip 
        (splitSD g d xs)
        (splitSD g d ys)

"unitEnv" proof
f :: PA () -> PA a -> PA a
f (replPA (lengthPA xs) ()) xs
  -- definition of lifted
  = (\(ATup0 n) xs -> body )
    (replPA (lengthPA xs) ())
    xs
  -- no need of Unit Environment-Variables
  = (\n xs -> body ) (lengthPA xs) xs



"Efficient Type dependent functions over Arrays"
class PR a where
  -- O(1)
  indexPA       :: PA a -> Int -> a
  lengthPA      :: PA a -> Int
  
  -- O(n)
  emptyPA       :: PA a
  replicatePA   :: Int -> a -> PA a
  
  -- =(sum lengths)
  replicatesPA  :: Segd -> PA a -> PA a
    
  -- O(slice len)
  extractPA,slicePA  :: PA a -> Int -> Int -> PA a
  
  -- > O(n)
  appendPA      :: PA a -> PA a -> PA a
  combine2PA    :: Sel2 -> PA a -> PA a -> PA a
  
  -- ?
  appendvsPA    :: U.Segd
                -> U.VSegd -> PAs a
                -> U.VSegd -> PAs a
                -> PA a
  indexsPA      :: PA (PA a) -> PA (Int, Int) -> PA a
  indexvsPA     :: PAs a -> U.VSegd -> U.Array (Int, Int) -> PA a
  extractssPA    :: PAs a -> U.SSegd -> PA a
  extractvsPA    :: PAs a -> U.VSegd -> PA a


"distributed definitions"
grep -r "^.*=.*[a-z+]D "


"Classifying Types in GHC." "https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TypeType#Classifyingtypes"
  "Algebraic = supports case deconstruction, e.g. ( a,b ) and (# a,b #)"
  "unlifted = cannot represent bottom, aka no pointer, e.g. ByteArray#"
  "unboxed = raw primitive value, unlifted & no pointer, e.g. Int#"

-- getting all rewrite rules from all files in the libraries
grep -rl "RULE" . | xargs -L1 gedit
http://askubuntu.com/questions/55325/how-to-use-grep-command-to-find-text-including-subdirectories
http://stackoverflow.com/questions/2711001/how-to-apply-shell-command-to-each-line-of-a-command-output
grep -r -A 7 "\-# RULE" .

