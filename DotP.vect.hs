
==================== Vectorisation ====================
Result size of Vectorisation
  = {terms: 1,106, types: 2,272, coercions: 589}

Rec { }
Rec {
dotp :: SparseVector -> Vector -> Double
dotp =
  \ (sv :: SparseVector) (v :: Vector) ->
    sumP
      (mapP
         (\ (ds :: (Double, Int)) ->
            case ds of _ { (d, i) -> * d (!: v i) })
         sv)

$vdotp
  :: V:GHC:PArr_[::] (Double, Int)
     :-> (V:GHC:PArr_[::] Double :-> Double)
$vdotp = closure $fPAVoid vdotp ldotp void

ldotp
  :: Int#
     -> PData Void
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double :-> Double)
ldotp =
  \ (lc :: Int#)
    (env :: PData Void)
    (arg :: PData (V:GHC:PArr_[::] (Double, Int))) ->
    ldotp lc arg

vdotp
  :: Void
     -> V:GHC:PArr_[::] (Double, Int)
     -> V:GHC:PArr_[::] Double :-> Double
vdotp =
  \ (env :: Void) (arg :: V:GHC:PArr_[::] (Double, Int)) -> vdotp arg

ldotp
  :: Int#
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double :-> Double)
ldotp =
  \ (lc :: Int#) (x :: PData (V:GHC:PArr_[::] (Double, Int))) ->
    liftedClosure
      ($fPAPArray
         (($fPRPArray
             ($p1PA
                ($fPA(,)
                   (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                   $fPADouble
                   $fPAInt)))
          `cast` ...)
         ($fPA(,)
            (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
            $fPADouble
            $fPAInt))
      vdotp
      ldotp
      x

vdotp
  :: V:GHC:PArr_[::] (Double, Int)
     -> V:GHC:PArr_[::] Double :-> Double
vdotp =
  \ (x :: V:GHC:PArr_[::] (Double, Int)) ->
    closure
      ($fPAPArray
         (($fPRPArray
             ($p1PA
                ($fPA(,)
                   (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                   $fPADouble
                   $fPAInt)))
          `cast` ...)
         ($fPA(,)
            (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
            $fPADouble
            $fPAInt))
      vdotp
      ldotp
      x

ldotp
  :: Int#
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData Double
ldotp =
  \ (lc :: Int#)
    (env :: PData (V:GHC:PArr_[::] (Double, Int)))
    (arg :: PData (V:GHC:PArr_[::] Double)) ->
    let {
      x :: PData (V:GHC:PArr_[::] (Double, Int))
      x = env } in
    ldotp lc x arg

vdotp
  :: V:GHC:PArr_[::] (Double, Int)
     -> V:GHC:PArr_[::] Double -> Double
vdotp =
  \ (env :: V:GHC:PArr_[::] (Double, Int))
    (arg :: V:GHC:PArr_[::] Double) ->
    let {
      x :: V:GHC:PArr_[::] (Double, Int)
      x = env } in
    vdotp x arg

ldotp
  :: Int#
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData Double
ldotp =
  \ (lc :: Int#)
    (sv :: PData (V:GHC:PArr_[::] (Double, Int)))
    (v :: PData (V:GHC:PArr_[::] Double)) ->
    liftedApply
      lc
      (replicatePD
         ($fPA:->
            (($fPR:->) `cast` ...)
            ($fPAPArray
               (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
            $fPADouble)
         lc
         $vsumP)
      (liftedApply
         lc
         (liftedApply
            lc
            (replicatePD
               ($fPA:->
                  (($fPR:->) `cast` ...)
                  ($fPA:->
                     (($fPR:->) `cast` ...)
                     ($fPA(,)
                        (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                        $fPADouble
                        $fPAInt)
                     $fPADouble)
                  ($fPA:->
                     (($fPR:->) `cast` ...)
                     ($fPAPArray
                        (($fPRPArray
                            ($p1PA
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt)))
                         `cast` ...)
                        ($fPA(,)
                           (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                           $fPADouble
                           $fPAInt))
                     ($fPAPArray
                        (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)))
               lc
               ($vmapP
                  ($fPA(,)
                     (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                     $fPADouble
                     $fPAInt)
                  $fPADouble))
            (liftedClosure
               ($fPAPArray
                  (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
               vdotp
               ldotp
               v))
         sv)

vdotp
  :: V:GHC:PArr_[::] (Double, Int)
     -> V:GHC:PArr_[::] Double -> Double
vdotp =
  \ (sv :: V:GHC:PArr_[::] (Double, Int))
    (v :: V:GHC:PArr_[::] Double) ->
    $:
      $vsumP
      ($:
         ($:
            ($vmapP
               ($fPA(,)
                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                  $fPADouble
                  $fPAInt)
               $fPADouble)
            (closure
               ($fPAPArray
                  (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
               vdotp
               ldotp
               v))
         sv)

ldotp
  :: Int#
     -> PData (V:GHC:PArr_[::] Double)
     -> PData (Double, Int)
     -> PData Double
ldotp =
  \ (lc :: Int#)
    (env :: PData (V:GHC:PArr_[::] Double))
    (arg :: PData (Double, Int)) ->
    let {
      v :: PData (V:GHC:PArr_[::] Double)
      v = env } in
    ldotp lc v arg

vdotp :: V:GHC:PArr_[::] Double -> (Double, Int) -> Double
vdotp =
  \ (env :: V:GHC:PArr_[::] Double) (arg :: (Double, Int)) ->
    let {
      v :: V:GHC:PArr_[::] Double
      v = env } in
    vdotp v arg

ldotp
  :: Int#
     -> PData (V:GHC:PArr_[::] Double)
     -> PData (Double, Int)
     -> PData Double
ldotp =
  \ (lc :: Int#)
    (v :: PData (V:GHC:PArr_[::] Double))
    (ds :: PData (Double, Int)) ->
    let {
      scrut :: PData (Double, Int)
      scrut = ds } in
    case scrut `cast` ... of wild { PTuple2 d i ->
    liftedApply
      lc
      (liftedApply
         lc
         (replicatePD
            ($fPA:->
               (($fPR:->) `cast` ...)
               $fPADouble
               ($fPA:-> (($fPR:->) `cast` ...) $fPADouble $fPADouble))
            lc
            $v*)
         d)
      (liftedApply
         lc
         (liftedApply
            lc
            (replicatePD
               ($fPA:->
                  (($fPR:->) `cast` ...)
                  ($fPAPArray
                     (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
                  ($fPA:-> (($fPR:->) `cast` ...) $fPAInt $fPADouble))
               lc
               ($v!: $fPADouble))
            v)
         i)
    }

vdotp :: V:GHC:PArr_[::] Double -> (Double, Int) -> Double
vdotp =
  \ (v :: V:GHC:PArr_[::] Double) (ds :: (Double, Int)) ->
    let {
      scrut :: (Double, Int)
      scrut = ds } in
    case scrut of wild { (d, i) ->
    $: ($: $v* d) ($: ($: ($v!: $fPADouble) v) i)
    }
end Rec }

Rec {
smvm :: SparseMatrix -> Vector -> Vector
smvm =
  \ (sm :: SparseMatrix) (v :: Vector) ->
    mapP (\ (ds :: SparseVector) -> dotp ds v) sm

$vsmvm
  :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     :-> (V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double)
$vsmvm = closure $fPAVoid vsmvm lsmvm void

lsmvm
  :: Int#
     -> PData Void
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double)
lsmvm =
  \ (lc :: Int#)
    (env :: PData Void)
    (arg :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))) ->
    lsmvm lc arg

vsmvm
  :: Void
     -> V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     -> V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double
vsmvm =
  \ (env :: Void)
    (arg :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))) ->
    vsmvm arg

lsmvm
  :: Int#
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double)
lsmvm =
  \ (lc :: Int#)
    (x :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))) ->
    liftedClosure
      ($fPAPArray
         (($fPRPArray
             ($p1PA
                ($fPAPArray
                   (($fPRPArray
                       ($p1PA
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                             $fPADouble
                             $fPAInt)))
                    `cast` ...)
                   ($fPA(,)
                      (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                      $fPADouble
                      $fPAInt))))
          `cast` ...)
         ($fPAPArray
            (($fPRPArray
                ($p1PA
                   ($fPA(,)
                      (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                      $fPADouble
                      $fPAInt)))
             `cast` ...)
            ($fPA(,)
               (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
               $fPADouble
               $fPAInt)))
      vsmvm
      lsmvm
      x

vsmvm
  :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     -> V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double
vsmvm =
  \ (x :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))) ->
    closure
      ($fPAPArray
         (($fPRPArray
             ($p1PA
                ($fPAPArray
                   (($fPRPArray
                       ($p1PA
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                             $fPADouble
                             $fPAInt)))
                    `cast` ...)
                   ($fPA(,)
                      (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                      $fPADouble
                      $fPAInt))))
          `cast` ...)
         ($fPAPArray
            (($fPRPArray
                ($p1PA
                   ($fPA(,)
                      (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                      $fPADouble
                      $fPAInt)))
             `cast` ...)
            ($fPA(,)
               (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
               $fPADouble
               $fPAInt)))
      vsmvm
      lsmvm
      x

lsmvm
  :: Int#
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData (V:GHC:PArr_[::] Double)
lsmvm =
  \ (lc :: Int#)
    (env :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))))
    (arg :: PData (V:GHC:PArr_[::] Double)) ->
    let {
      x :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
      x = env } in
    lsmvm lc x arg

vsmvm
  :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     -> V:GHC:PArr_[::] Double -> V:GHC:PArr_[::] Double
vsmvm =
  \ (env :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
    (arg :: V:GHC:PArr_[::] Double) ->
    let {
      x :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
      x = env } in
    vsmvm x arg

lsmvm
  :: Int#
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData (V:GHC:PArr_[::] Double)
lsmvm =
  \ (lc :: Int#)
    (sm :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))))
    (v :: PData (V:GHC:PArr_[::] Double)) ->
    liftedApply
      lc
      (liftedApply
         lc
         (replicatePD
            ($fPA:->
               (($fPR:->) `cast` ...)
               ($fPA:->
                  (($fPR:->) `cast` ...)
                  ($fPAPArray
                     (($fPRPArray
                         ($p1PA
                            ($fPA(,)
                               (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                               $fPADouble
                               $fPAInt)))
                      `cast` ...)
                     ($fPA(,)
                        (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                        $fPADouble
                        $fPAInt))
                  $fPADouble)
               ($fPA:->
                  (($fPR:->) `cast` ...)
                  ($fPAPArray
                     (($fPRPArray
                         ($p1PA
                            ($fPAPArray
                               (($fPRPArray
                                   ($p1PA
                                      ($fPA(,)
                                         (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt))
                                          `cast` ...)
                                         $fPADouble
                                         $fPAInt)))
                                `cast` ...)
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt))))
                      `cast` ...)
                     ($fPAPArray
                        (($fPRPArray
                            ($p1PA
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt)))
                         `cast` ...)
                        ($fPA(,)
                           (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                           $fPADouble
                           $fPAInt)))
                  ($fPAPArray
                     (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)))
            lc
            ($vmapP
               ($fPAPArray
                  (($fPRPArray
                      ($p1PA
                         ($fPA(,)
                            (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                            $fPADouble
                            $fPAInt)))
                   `cast` ...)
                  ($fPA(,)
                     (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                     $fPADouble
                     $fPAInt))
               $fPADouble))
         (liftedClosure
            ($fPAPArray
               (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
            vsmvm
            lsmvm
            v))
      sm

vsmvm
  :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     -> V:GHC:PArr_[::] Double -> V:GHC:PArr_[::] Double
vsmvm =
  \ (sm :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
    (v :: V:GHC:PArr_[::] Double) ->
    $:
      ($:
         ($vmapP
            ($fPAPArray
               (($fPRPArray
                   ($p1PA
                      ($fPA(,)
                         (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                         $fPADouble
                         $fPAInt)))
                `cast` ...)
               ($fPA(,)
                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                  $fPADouble
                  $fPAInt))
            $fPADouble)
         (closure
            ($fPAPArray
               (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
            vsmvm
            lsmvm
            v))
      sm

lsmvm
  :: Int#
     -> PData (V:GHC:PArr_[::] Double)
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData Double
lsmvm =
  \ (lc :: Int#)
    (env :: PData (V:GHC:PArr_[::] Double))
    (arg :: PData (V:GHC:PArr_[::] (Double, Int))) ->
    let {
      v :: PData (V:GHC:PArr_[::] Double)
      v = env } in
    lsmvm lc v arg

vsmvm
  :: V:GHC:PArr_[::] Double
     -> V:GHC:PArr_[::] (Double, Int) -> Double
vsmvm =
  \ (env :: V:GHC:PArr_[::] Double)
    (arg :: V:GHC:PArr_[::] (Double, Int)) ->
    let {
      v :: V:GHC:PArr_[::] Double
      v = env } in
    vsmvm v arg

lsmvm
  :: Int#
     -> PData (V:GHC:PArr_[::] Double)
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData Double
lsmvm =
  \ (lc :: Int#)
    (v :: PData (V:GHC:PArr_[::] Double))
    (ds :: PData (V:GHC:PArr_[::] (Double, Int))) ->
    liftedApply
      lc
      (liftedApply
         lc
         (replicatePD
            ($fPA:->
               (($fPR:->) `cast` ...)
               ($fPAPArray
                  (($fPRPArray
                      ($p1PA
                         ($fPA(,)
                            (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                            $fPADouble
                            $fPAInt)))
                   `cast` ...)
                  ($fPA(,)
                     (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                     $fPADouble
                     $fPAInt))
               ($fPA:->
                  (($fPR:->) `cast` ...)
                  ($fPAPArray
                     (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
                  $fPADouble))
            lc
            $vdotp)
         ds)
      v

vsmvm
  :: V:GHC:PArr_[::] Double
     -> V:GHC:PArr_[::] (Double, Int) -> Double
vsmvm =
  \ (v :: V:GHC:PArr_[::] Double)
    (ds :: V:GHC:PArr_[::] (Double, Int)) ->
    $: ($: $vdotp ds) v
end Rec }

Rec {
example :: Double
example = $vexample

$vexample :: Double
$vexample =
  $:
    $vsumP
    ($:
       ($:
          $vsmvm
          ($:
             ($:
                ($v+:+
                   ($fPAPArray
                      (($fPRPArray
                          ($p1PA
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                $fPADouble
                                $fPAInt)))
                       `cast` ...)
                      ($fPA(,)
                         (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                         $fPADouble
                         $fPAInt)))
                ($:
                   ($vsingletonP
                      ($fPAPArray
                         (($fPRPArray
                             ($p1PA
                                ($fPA(,)
                                   (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                   $fPADouble
                                   $fPAInt)))
                          `cast` ...)
                         ($fPA(,)
                            (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                            $fPADouble
                            $fPAInt)))
                   ($:
                      ($:
                         ($v+:+
                            ($fPA(,)
                               (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                               $fPADouble
                               $fPAInt))
                         ($:
                            ($vsingletonP
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt))
                            ($: ($: (tup2 $fPADouble $fPAInt) (D# 3.1)) (I# 1))))
                      ($:
                         ($:
                            ($v+:+
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt))
                            ($:
                               ($vsingletonP
                                  ($fPA(,)
                                     (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                     $fPADouble
                                     $fPAInt))
                               ($: ($: (tup2 $fPADouble $fPAInt) (D# 3.2)) (I# 2))))
                         ($:
                            ($vsingletonP
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt))
                            ($: ($: (tup2 $fPADouble $fPAInt) (D# 2.1)) (I# 5)))))))
             ($:
                ($:
                   ($v+:+
                      ($fPAPArray
                         (($fPRPArray
                             ($p1PA
                                ($fPA(,)
                                   (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                   $fPADouble
                                   $fPAInt)))
                          `cast` ...)
                         ($fPA(,)
                            (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                            $fPADouble
                            $fPAInt)))
                   ($:
                      ($vsingletonP
                         ($fPAPArray
                            (($fPRPArray
                                ($p1PA
                                   ($fPA(,)
                                      (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt))
                                       `cast` ...)
                                      $fPADouble
                                      $fPAInt)))
                             `cast` ...)
                            ($fPA(,)
                               (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                               $fPADouble
                               $fPAInt)))
                      ($:
                         ($:
                            ($v+:+
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt))
                            ($:
                               ($vsingletonP
                                  ($fPA(,)
                                     (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                     $fPADouble
                                     $fPAInt))
                               ($: ($: (tup2 $fPADouble $fPAInt) (D# 5.5)) (I# 0))))
                         ($:
                            ($vsingletonP
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt))
                            ($: ($: (tup2 $fPADouble $fPAInt) (D# 1.4)) (I# 2))))))
                ($:
                   ($:
                      ($v+:+
                         ($fPAPArray
                            (($fPRPArray
                                ($p1PA
                                   ($fPA(,)
                                      (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt))
                                       `cast` ...)
                                      $fPADouble
                                      $fPAInt)))
                             `cast` ...)
                            ($fPA(,)
                               (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                               $fPADouble
                               $fPAInt)))
                      ($:
                         ($vsingletonP
                            ($fPAPArray
                               (($fPRPArray
                                   ($p1PA
                                      ($fPA(,)
                                         (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt))
                                          `cast` ...)
                                         $fPADouble
                                         $fPAInt)))
                                `cast` ...)
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt)))
                         ($vemptyP
                            ($fPA(,)
                               (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                               $fPADouble
                               $fPAInt))))
                   ($:
                      ($vsingletonP
                         ($fPAPArray
                            (($fPRPArray
                                ($p1PA
                                   ($fPA(,)
                                      (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt))
                                       `cast` ...)
                                      $fPADouble
                                      $fPAInt)))
                             `cast` ...)
                            ($fPA(,)
                               (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                               $fPADouble
                               $fPAInt)))
                      ($:
                         ($:
                            ($v+:+
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                  $fPADouble
                                  $fPAInt))
                            ($:
                               ($vsingletonP
                                  ($fPA(,)
                                     (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                     $fPADouble
                                     $fPAInt))
                               ($: ($: (tup2 $fPADouble $fPAInt) (D# 21.1)) (I# 0))))
                         ($:
                            ($:
                               ($v+:+
                                  ($fPA(,)
                                     (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt)) `cast` ...)
                                     $fPADouble
                                     $fPAInt))
                               ($:
                                  ($vsingletonP
                                     ($fPA(,)
                                        (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt))
                                         `cast` ...)
                                        $fPADouble
                                        $fPAInt))
                                  ($: ($: (tup2 $fPADouble $fPAInt) (D# 31.2)) (I# 1))))
                            ($:
                               ($:
                                  ($v+:+
                                     ($fPA(,)
                                        (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt))
                                         `cast` ...)
                                        $fPADouble
                                        $fPAInt))
                                  ($:
                                     ($vsingletonP
                                        ($fPA(,)
                                           (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt))
                                            `cast` ...)
                                           $fPADouble
                                           $fPAInt))
                                     ($: ($: (tup2 $fPADouble $fPAInt) (D# 1.1)) (I# 3))))
                               ($:
                                  ($vsingletonP
                                     ($fPA(,)
                                        (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPAInt))
                                         `cast` ...)
                                        $fPADouble
                                        $fPAInt))
                                  ($: ($: (tup2 $fPADouble $fPAInt) (D# 2.1)) (I# 4)))))))))))
       ($:
          ($: ($v+:+ $fPADouble) ($: ($vsingletonP $fPADouble) (D# 2.4)))
          ($:
             ($: ($v+:+ $fPADouble) ($: ($vsingletonP $fPADouble) (D# 4.2)))
             ($:
                ($: ($v+:+ $fPADouble) ($: ($vsingletonP $fPADouble) (D# 1.2)))
                ($:
                   ($: ($v+:+ $fPADouble) ($: ($vsingletonP $fPADouble) (D# 2.3)))
                   ($:
                      ($: ($v+:+ $fPADouble) ($: ($vsingletonP $fPADouble) (D# 6.5)))
                      ($: ($vsingletonP $fPADouble) (D# 0.3))))))))
end Rec }




==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 10,893, types: 13,147, coercions: 4,868}

dotp1 :: Int
dotp1 = absentError "ww{v} [lid] Int{(w) tc 3J}"

$wdotp :: Array# (Double, Int) -> Vector -> Double
$wdotp =
  \ (ww :: Array# (Double, Int)) (w :: Vector) ->
    sumP
      (mapP
         (\ (ds :: (Double, Int)) ->
            case ds of _ { (d, i) ->
            case d of _ { D# x -> case !: w i of _ { D# y -> D# (*## x y) } }
            })
         (PArr dotp1 ww))

dotp :: SparseVector -> Vector -> Double
dotp =
  \ (w :: SparseVector) (w1 :: Vector) ->
    case w of _ { PArr ww ww1 -> $wdotp ww1 w1 }

$vexample5 :: [Char]
$vexample5 = unpackCString# "foldUP/f"

lvl :: What
lvl = What $vexample5

$vexample3 :: [Char]
$vexample3 = unpackCString# "foldUP/fold"

lvl1 :: What
lvl1 = What $vexample3

lvl2 :: [Char]
lvl2 = unpackCString# "zipWithUP/map"

lvl3 :: What
lvl3 = What lvl2

lvl4 :: What
lvl4 = WFMapMap lvl1 lvl3

lvl5 :: What
lvl5 = WFMapGen lvl4 WSlice

lvl6 :: [Char]
lvl6 = unpackCString# "./Data/Vector/Generic/Mutable.hs"

lvl7 :: Int
lvl7 = I# 495

lvl8 :: [Char]
lvl8 = unpackCString# "new"

lvl9 :: forall s. Int# -> ST s (MVector (PrimState (ST s)) Int)
lvl9 =
  \ (@ s) (ipv5 :: Int#) ->
    checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)

$dDT1 :: DT (Vector Int)
$dDT1 = $fDTVector $fUnboxInt

lvl10 :: DT (Int, Vector Int)
lvl10 = $fDT(,) $fDTInt $dDT1

lvl11 :: [Char]
lvl11 = unpackCString# "joinD_impl/count"

lvl12 :: What
lvl12 = What lvl11

lvl13 :: Int
lvl13 = I# 0

lvl14 :: DT ((Int, Int), Vector Int)
lvl14 = $fDT(,) enumFromStepLenUP_$dDT $dDT1

lvl15 :: [Char]
lvl15 = unpackCString# "mapUP/map"

lvl16 :: What
lvl16 = What lvl15

lvl17 :: What
lvl17 = What lvl15

lvl18 :: [Char]
lvl18 = unpackCString# "enumFromStepLenUP/gen"

lvl19 :: What
lvl19 = What lvl18

lvl20 :: What
lvl20 = WFMapMap lvl17 lvl19

lvl21 :: What
lvl21 = WFMapMap WZip lvl20

lvl22 :: What
lvl22 = WFMapMap lvl16 lvl21

lvl23 :: Int -> ((Int, Int), Vector Int) -> Vector Int
lvl23 =
  \ _ (x :: ((Int, Int), Vector Int)) ->
    case x of _ { (x1, y) ->
    case x1 of _ { (x2, y1) ->
    case x2 of _ { I# ipv ->
    case y1 of _ { I# ipv1 ->
    case y `cast` ... of _ { Vector ipv2 ipv3 ipv4 ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $w$j :: Int# -> (# State# s, Vector Int #)
           $w$j =
             \ (w :: Int#) ->
               let {
                 $j :: Int# -> (# State# s, Vector Int #)
                 $j =
                   \ (x3 :: Int#) ->
                     case newByteArray# (*# x3 4) (s `cast` ...)
                     of _ { (# ipv5, ipv6 #) ->
                     letrec {
                       $s$wa
                         :: Int# -> State# s -> Int# -> Int# -> Int# -> (# State# s, Int #)
                       $s$wa =
                         \ (sc :: Int#)
                           (sc1 :: State# s)
                           (sc2 :: Int#)
                           (sc3 :: Int#)
                           (sc4 :: Int#) ->
                           case ># sc3 0 of _ {
                             False -> (# sc1, I# sc #);
                             True ->
                               case >=# sc4 ipv3 of _ {
                                 False ->
                                   case indexIntArray# ipv4 (+# ipv2 sc4) of wild4 { __DEFAULT ->
                                   case writeIntArray# ipv6 sc wild4 (sc1 `cast` ...)
                                   of s'# { __DEFAULT ->
                                   $s$wa (+# sc 1) (s'# `cast` ...) (+# sc2 1) (-# sc3 1) (+# sc4 1)
                                   }
                                   };
                                 True -> (# sc1, I# sc #)
                               }
                           }; } in
                     case $s$wa 0 (ipv5 `cast` ...) ipv1 ipv 0
                     of _ { (# ipv7, ipv8 #) ->
                     case ipv8 of _ { I# tpl3 ->
                     case unsafeFreezeByteArray# ipv6 (ipv7 `cast` ...)
                     of _ { (# ipv9, ipv10 #) ->
                     (# ipv9 `cast` ..., (Vector 0 tpl3 ipv10) `cast` ... #)
                     }
                     }
                     }
                     } } in
               case <=# w ipv3 of _ {
                 False -> $j ipv3;
                 True -> $j w
               } } in
         case <=# ipv 0 of _ {
           False -> $w$j ipv;
           True -> $w$j 0
         })
    }
    }
    }
    }
    }

lvl24 :: Dist (Int, Int)
lvl24 = splitLenIdxD theGang lvl13

$wvdotp :: PData (Double, Int) -> PData Double -> Double
$wvdotp =
  \ (ww :: PData (Double, Int)) (ww1 :: PData Double) ->
    case ww `cast` ... of _ { PTuple2 d i ->
    case d `cast` ... of _ { PDouble xs ->
    case i `cast` ... of _ { PInt ipv4 ->
    case ww1 `cast` ... of _ { PDouble pdata ->
    case ipv4 `cast` ... of _ { Vector rb rb1 rb2 ->
    case xs `cast` ... of _ { Vector ipv ipv1 ipv2 ->
    case theGang of wild4 { Gang rb3 ds1 ds2 ->
    let {
      x1 :: Int#
      x1 = +# (-# rb1 1) 1 } in
    case case <=# x1 0 of _ {
           False -> (splitLenIdxD wild4 (I# x1)) `cast` ...;
           True -> lvl24 `cast` ...
         }
    of nt2 { DProd ipv3 ipv5 ->
    case quotInt# rb1 rb3 of wild5 { __DEFAULT ->
    case remInt# rb1 rb3 of wild6 { __DEFAULT ->
    case (generateD_cheap
            $dDT1
            WSlice
            wild4
            (\ (i1 :: Int) ->
               case i1 of _ { I# x2 ->
               case <# x2 wild6 of _ {
                 False ->
                   (Vector (+# rb (+# (*# wild5 x2) wild6)) wild5 rb2) `cast` ...;
                 True ->
                   (Vector (+# rb (*# (+# wild5 1) x2)) (+# wild5 1) rb2) `cast` ...
               }
               }))
         `cast` ...
    of nt3 { __DEFAULT ->
    case (imapD'
            lvl14
            $dDT1
            lvl22
            wild4
            lvl23
            ((DProd (nt2 `cast` ...) (nt3 `cast` ...)) `cast` ...))
         `cast` ...
    of nt4 { DVector ipv6 ipv7 ipv8 ipv9 ->
    case (runSTRep
            (\ (@ s) (s :: State# s) ->
               case scanD $fDTInt lvl12 wild4 $fNumInt_$c+ lvl13 ipv6
               of _ { (di, n) ->
               case di `cast` ... of nt5 { DInt ipv10 ->
               case n of n1 { I# ipv11 ->
               case >=# ipv11 0 of _ {
                 False -> case lvl9 ipv11 of wild9 { };
                 True ->
                   case newByteArray# (*# ipv11 4) (s `cast` ...)
                   of _ { (# ipv12, ipv13 #) ->
                   let {
                     nt6 :: R:Dist(,) Int (Vector Int)
                     nt6 = DProd (nt5 `cast` ...) (nt4 `cast` ...) } in
                   case ($wa1
                           rb3
                           ds1
                           ds2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i1 :: Int) (s1 :: State# s) ->
                               case ((((myD lvl10 (nt6 `cast` ...)) `cast` ...) i1) `cast` ...) s1
                               of _ { (# ipv14, ipv15 #) ->
                               case ipv15 of _ { (x, y) ->
                               case x of _ { I# ipv16 ->
                               case y `cast` ... of _ { Vector ipv17 ipv18 ipv19 ->
                               case copyByteArray#
                                      ipv19
                                      (*# ipv17 4)
                                      ipv13
                                      (*# ipv16 4)
                                      (*# ipv18 4)
                                      (ipv14 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv12 `cast` ...))
                        `cast` ...
                   of _ { (# ipv14, _ #) ->
                   case unsafeFreezeByteArray# ipv13 (ipv14 `cast` ...)
                   of _ { (# ipv16, ipv17 #) ->
                   (# ipv16 `cast` ..., (Vector 0 ipv11 ipv17) `cast` ... #)
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv10 ipv11 ipv12 ->
    case pdata `cast` ... of _ { Vector ipv13 _ ipv15 ->
    case (runSTRep
            (\ (@ s) (s :: State# s) ->
               case newByteArray# (*# ipv11 8) (s `cast` ...)
               of _ { (# ipv16, ipv17 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case >=# sc1 ipv11 of _ {
                       False ->
                         case indexIntArray# ipv12 (+# ipv10 sc1) of wild8 { __DEFAULT ->
                         case indexDoubleArray# ipv15 (+# ipv13 wild8)
                         of wild9 { __DEFAULT ->
                         case writeDoubleArray# ipv17 sc wild9 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                         }
                         }
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 0 (ipv16 `cast` ...) of _ { (# ipv18, ipv19 #) ->
               case ipv19 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv17 (ipv18 `cast` ...)
               of _ { (# ipv20, ipv21 #) ->
               (# ipv20 `cast` ..., (Vector 0 tpl1 ipv21) `cast` ... #)
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv16 ipv17 ipv18 ->
    let {
      $j :: Int# -> Double
      $j =
        \ (tpl1 :: Int#) ->
          case quotInt# tpl1 rb3 of wild7 { __DEFAULT ->
          case remInt# tpl1 rb3 of wild8 { __DEFAULT ->
          foldD
            $fDTDouble
            lvl
            wild4
            plusDouble
            (generateD
               $fDTDouble
               lvl5
               wild4
               ((\ (i1 :: Int) ->
                   case i1 of _ { I# x2 ->
                   let {
                     $w$j :: Int# -> Double#
                     $w$j =
                       \ (w :: Int#) ->
                         let {
                           $w$j1 :: Int# -> Double#
                           $w$j1 =
                             \ (w1 :: Int#) ->
                               let {
                                 a4 :: Int#
                                 a4 = +# ipv16 w1 } in
                               let {
                                 a5 :: Int#
                                 a5 = +# ipv w1 } in
                               letrec {
                                 $s$wfoldlM'_loop :: Double# -> Int# -> Double#
                                 $s$wfoldlM'_loop =
                                   \ (sc :: Double#) (sc1 :: Int#) ->
                                     case >=# sc1 w of _ {
                                       False ->
                                         case indexDoubleArray# ipv2 (+# a5 sc1)
                                         of wild11 { __DEFAULT ->
                                         case indexDoubleArray# ipv18 (+# a4 sc1)
                                         of wild12 { __DEFAULT ->
                                         $s$wfoldlM'_loop (+## sc (*## wild11 wild12)) (+# sc1 1)
                                         }
                                         };
                                       True -> sc
                                     }; } in
                               $s$wfoldlM'_loop 0.0 0 } in
                         case <# x2 wild8 of _ {
                           False -> $w$j1 (+# (*# wild7 x2) wild8);
                           True -> $w$j1 (*# (+# wild7 1) x2)
                         } } in
                   case <# x2 wild8 of _ {
                     False ->
                       case $w$j wild7 of ww2 { __DEFAULT -> (D# ww2) `cast` ... };
                     True ->
                       case $w$j (+# wild7 1) of ww2 { __DEFAULT -> (D# ww2) `cast` ... }
                   }
                   })
                `cast` ...))
          }
          } } in
    case <=# ipv1 ipv17 of _ {
      False -> $j ipv17;
      True -> $j ipv1
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vdotp6
  :: V:GHC:PArr_[::] (Double, Int)
     -> V:GHC:PArr_[::] Double -> Double
$vdotp6 =
  \ (w :: V:GHC:PArr_[::] (Double, Int))
    (w1 :: V:GHC:PArr_[::] Double) ->
    case w of _ { PArray ww ww1 ->
    case w1 of _ { PArray ww2 ww3 -> $wvdotp ww1 ww3 }
    }

$vdotp5
  :: V:GHC:PArr_[::] (Double, Int)
     -> V:GHC:PArr_[::] Double -> Double
$vdotp5 = $vdotp6

lvl25 :: forall s. Int# -> ST s (MVector (PrimState (ST s)) Double)
lvl25 =
  \ (@ s) (ipv5 :: Int#) ->
    checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)

$dDT2 :: DT (Vector Double)
$dDT2 = $fDTVector $fUnboxDouble

lvl26 :: DT (Int, Vector Double)
lvl26 = $fDT(,) $fDTInt $dDT2

lvl27 :: What
lvl27 = What lvl11

lvl28 :: DT ((Int, Int), Vector Int)
lvl28 = $fDT(,) enumFromStepLenUP_$dDT $dDT1

lvl29 :: DT (Vector Double, ((Int, Int), Vector Int))
lvl29 = $fDT(,) $dDT2 lvl28

lvl30 :: What
lvl30 = What lvl2

lvl31 :: [Char]
lvl31 = unpackCString# "indexsFromVectorsUPVSegdP"

lvl32 :: What
lvl32 = What lvl31

lvl33 :: What
lvl33 = What lvl15

lvl34 :: What
lvl34 = What lvl18

lvl35 :: What
lvl35 = WFMapMap lvl33 lvl34

lvl36 :: What
lvl36 = WFMapMap WZip lvl35

lvl37 :: What
lvl37 = WFMapMap lvl32 lvl36

lvl38 :: What
lvl38 = WFMapMap WZip lvl37

lvl39 :: What
lvl39 = WFMapMap lvl30 lvl38

lvl40 :: Dist (Int, Int)
lvl40 = splitLenIdxD theGang lvl13

lvl41 :: forall s. Int# -> ST s (MVector (PrimState (ST s)) Double)
lvl41 =
  \ (@ s) (ipv5 :: Int#) ->
    checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)

lvl42 :: DT (Int, Vector Double)
lvl42 = $fDT(,) $fDTInt $dDT2

lvl43 :: What
lvl43 = What lvl11

lvl44 :: What
lvl44 = WFMapGen WBpermute WSlice

file :: String
file = unpackCString# "./Data/Vector/Generic.hs"

lvl45 :: Int
lvl45 = I# 924

lvl46 :: [Char]
lvl46 = unpackCString# "backpermute"

lvl47 :: Int# -> Int# -> Box Double
lvl47 =
  \ (wild :: Int#) (ipv :: Int#) ->
    checkError file lvl45 Bounds lvl46 (checkIndex_msg# wild ipv)

lvl48 :: Int# -> Int# -> Box Double
lvl48 =
  \ (wild :: Int#) (ipv :: Int#) ->
    checkError file lvl45 Bounds lvl46 (checkIndex_msg# wild ipv)

lvl49 :: forall s. Int# -> ST s (MVector (PrimState (ST s)) Double)
lvl49 =
  \ (@ s) (ipv7 :: Int#) ->
    checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv7)

lvl50 :: DT (Int, Vector Double)
lvl50 = $fDT(,) $fDTInt $dDT2

lvl51 :: [Char]
lvl51 = unpackCString# "joinDM/count"

lvl52 :: What
lvl52 = What lvl51

lvl53 :: DT (Int, Vector Double)
lvl53 = $fDT(,) $fDTInt $dDT2

lvl54 :: DT ((Int, Vector Double), Vector Double)
lvl54 = $fDT(,) lvl53 $dDT2

lvl55 :: [Char]
lvl55 = unpackCString# "UPSSegd.foldSegsWithP/partial"

lvl56 :: What
lvl56 = What lvl55

lvl57 :: State# RealWorld -> (# State# RealWorld, ByteArray #)
lvl57 =
  \ (s :: State# RealWorld) ->
    case noDuplicate# s of s' { __DEFAULT ->
    case newByteArray# 0 (s' `cast` ...) of _ { (# ipv10, ipv11 #) ->
    case unsafeFreezeByteArray# ipv11 ipv10
    of _ { (# ipv12, ipv13 #) ->
    (# ipv12, ByteArray ipv13 #) `cast` ...
    }
    }
    }

lvl58 :: ByteArray
lvl58 = unsafeDupablePerformIO (lvl57 `cast` ...)

lvl59 :: [Char]
lvl59 = unpackCString# "./Data/Vector/Fusion/Stream/Monadic.hs"

lvl60 :: Int
lvl60 = I# 1283

lvl61 :: [Char]
lvl61 = unpackCString# "enumFromTo"

lvl62 :: [Char]
lvl62 = unpackCString# "vector too large"

lvl63 :: Int
lvl63 = checkError lvl59 lvl60 Bounds lvl61 lvl62

$wldotp
  :: PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double) -> PData Double
$wldotp =
  \ (w :: PData (V:GHC:PArr_[::] (Double, Int)))
    (w1 :: PData (V:GHC:PArr_[::] Double)) ->
    case w `cast` ... of _ { PNested _ _ ipv6 ipv7 ->
    case ipv6 of segd1 { UPSegd ipv ipv1 ipv2 ipv3 ->
    case ipv `cast` ... of wild { Vector rb rb1 rb2 ->
    case ipv7 `cast` ... of _ { PTuple2 as bs ->
    case as `cast` ... of _ { PDouble uarr ->
    case uarr `cast` ... of _ { Vector rb3 rb4 rb5 ->
    let {
      a4 :: Int#
      a4 = -# rb1 1 } in
    case (runSTRep
            (\ (@ s) (s :: State# s) ->
               let {
                 $j :: Int# -> (# State# s, Vector Int #)
                 $j =
                   \ (x :: Int#) ->
                     case newByteArray# (*# x 4) (s `cast` ...)
                     of _ { (# ipv8, ipv9 #) ->
                     letrec {
                       $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                       $s$wa =
                         \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                           case <=# sc1 a4 of _ {
                             False -> (# sc2, I# sc #);
                             True ->
                               case writeIntArray# ipv9 sc sc1 (sc2 `cast` ...)
                               of s'# { __DEFAULT ->
                               $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                               }
                           }; } in
                     case $s$wa 0 0 (ipv8 `cast` ...) of _ { (# ipv10, ipv11 #) ->
                     case ipv11 of _ { I# tpl1 ->
                     case unsafeFreezeByteArray# ipv9 (ipv10 `cast` ...)
                     of _ { (# ipv12, ipv13 #) ->
                     (# ipv12 `cast` ..., (Vector 0 tpl1 ipv13) `cast` ... #)
                     }
                     }
                     }
                     } } in
               case ># 0 a4 of _ {
                 False ->
                   let {
                     a5 :: Int#
                     a5 = +# a4 1 } in
                   case ># a5 0 of _ {
                     False -> case lvl63 of wild6 { };
                     True -> $j a5
                   };
                 True -> $j 0
               }))
         `cast` ...
    of _ { Vector ipv8 ipv9 ipv10 ->
    let {
      arrs :: Arrays Double
      arrs =
        case w1 `cast` ... of _ { PNested vsegd xs _ _ ->
        case bs `cast` ... of _ { PInt ipv11 ->
        case xs `cast` ... of _ { PDoubles arrs1 ->
        case ipv11 `cast` ... of _ { Vector rb6 rb7 rb8 ->
        runSTRep
          (\ (@ s) (s :: State# s) ->
             case theGang of wild7 { Gang rb9 ds1 ds2 ->
             case quotInt# rb4 rb9 of wild8 { __DEFAULT ->
             case remInt# rb4 rb9 of wild9 { __DEFAULT ->
             case (generateD_cheap
                     $dDT2
                     WSlice
                     wild7
                     (\ (i :: Int) ->
                        case i of _ { I# x1 ->
                        case <# x1 wild9 of _ {
                          False ->
                            (Vector (+# rb3 (+# (*# wild8 x1) wild9)) wild8 rb5) `cast` ...;
                          True ->
                            (Vector (+# rb3 (*# (+# wild8 1) x1)) (+# wild8 1) rb5) `cast` ...
                        }
                        }))
                  `cast` ...
             of nt3 { __DEFAULT ->
             let {
               x1 :: Int#
               x1 = +# (-# rb7 1) 1 } in
             case case <=# x1 0 of _ {
                    False -> (splitLenIdxD wild7 (I# x1)) `cast` ...;
                    True -> lvl40 `cast` ...
                  }
             of nt4 { DProd ipv12 ipv13 ->
             case quotInt# rb7 rb9 of wild10 { __DEFAULT ->
             case remInt# rb7 rb9 of wild11 { __DEFAULT ->
             case (generateD_cheap
                     $dDT1
                     WSlice
                     wild7
                     (\ (i :: Int) ->
                        case i of _ { I# x2 ->
                        case <# x2 wild11 of _ {
                          False ->
                            (Vector (+# rb6 (+# (*# wild10 x2) wild11)) wild10 rb8) `cast` ...;
                          True ->
                            (Vector (+# rb6 (*# (+# wild10 1) x2)) (+# wild10 1) rb8)
                            `cast` ...
                        }
                        }))
                  `cast` ...
             of nt5 { __DEFAULT ->
             let {
               upvsegd :: UPVSegd
               upvsegd = updateVSegs (replicate_s $fEltInt segd1) vsegd } in
             case (imapD'
                     lvl29
                     $dDT2
                     lvl39
                     wild7
                     (\ _ (x :: (Vector Double, ((Int, Int), Vector Int))) ->
                        case x of _ { (x2, y) ->
                        case x2 `cast` ... of _ { Vector ipv14 ipv15 ipv16 ->
                        case y of _ { (x3, y1) ->
                        case x3 of _ { (x4, y2) ->
                        case x4 of _ { I# ipv17 ->
                        case y2 of _ { I# ipv18 ->
                        case y1 `cast` ... of _ { Vector ipv19 ipv20 ipv21 ->
                        case upvsegd of _ { UPVSegd ds3 ds4 ds5 ds6 ds7 ds8 ->
                        case ds6
                        of _ { UPSSegd ipv22 ipv23 ipv24 ipv25 ipv26 ipv27 ipv28 ->
                        case ds4 `cast` ... of _ { Vector ipv29 _ ipv31 ->
                        case ipv23 `cast` ... of _ { Vector ipv32 _ ipv34 ->
                        case ipv24 `cast` ... of _ { Vector ipv35 _ ipv37 ->
                        case arrs1 of _ { Vectors ipv38 ipv39 ipv40 ipv41 ->
                        runSTRep
                          (\ (@ s1) (s1 :: State# s1) ->
                             let {
                               $w$j :: Int# -> (# State# s1, Vector Double #)
                               $w$j =
                                 \ (w2 :: Int#) ->
                                   let {
                                     $w$j1 :: Int# -> (# State# s1, Vector Double #)
                                     $w$j1 =
                                       \ (w3 :: Int#) ->
                                         let {
                                           $j :: Int# -> (# State# s1, Vector Double #)
                                           $j =
                                             \ (x5 :: Int#) ->
                                               case newByteArray# (*# x5 8) (s1 `cast` ...)
                                               of _ { (# ipv42, ipv43 #) ->
                                               letrec {
                                                 $s$wa
                                                   :: Int#
                                                      -> State# s1
                                                      -> Int#
                                                      -> Int#
                                                      -> Int#
                                                      -> Int#
                                                      -> (# State# s1, Int #)
                                                 $s$wa =
                                                   \ (sc :: Int#)
                                                     (sc1 :: State# s1)
                                                     (sc2 :: Int#)
                                                     (sc3 :: Int#)
                                                     (sc4 :: Int#)
                                                     (sc5 :: Int#) ->
                                                     case >=# sc2 ipv15 of _ {
                                                       False ->
                                                         case indexDoubleArray# ipv16 (+# ipv14 sc2)
                                                         of wild17 { __DEFAULT ->
                                                         case ># sc4 0 of _ {
                                                           False -> (# sc1, I# sc #);
                                                           True ->
                                                             case >=# sc5 ipv20 of _ {
                                                               False ->
                                                                 case indexIntArray#
                                                                        ipv31 (+# ipv29 sc3)
                                                                 of wild20 { __DEFAULT ->
                                                                 case indexIntArray#
                                                                        ipv37 (+# ipv35 wild20)
                                                                 of wild21 { __DEFAULT ->
                                                                 case indexIntArray#
                                                                        ipv21 (+# ipv19 sc5)
                                                                 of wild22 { __DEFAULT ->
                                                                 case indexIntArray#
                                                                        ipv34 (+# ipv32 wild20)
                                                                 of wild23 { __DEFAULT ->
                                                                 case indexIntArray# ipv39 wild21
                                                                 of wild24 { __DEFAULT ->
                                                                 case indexByteArrayArray#
                                                                        ipv41 wild21
                                                                 of wild25 { __DEFAULT ->
                                                                 case indexDoubleArray#
                                                                        wild25
                                                                        (+#
                                                                           wild24
                                                                           (+# wild23 wild22))
                                                                 of wild26 { __DEFAULT ->
                                                                 case writeDoubleArray#
                                                                        ipv43
                                                                        sc
                                                                        (*## wild17 wild26)
                                                                        (sc1 `cast` ...)
                                                                 of s'# { __DEFAULT ->
                                                                 $s$wa
                                                                   (+# sc 1)
                                                                   (s'# `cast` ...)
                                                                   (+# sc2 1)
                                                                   (+# sc3 1)
                                                                   (-# sc4 1)
                                                                   (+# sc5 1)
                                                                 }
                                                                 }
                                                                 }
                                                                 }
                                                                 }
                                                                 }
                                                                 }
                                                                 };
                                                               True -> (# sc1, I# sc #)
                                                             }
                                                         }
                                                         };
                                                       True -> (# sc1, I# sc #)
                                                     }; } in
                                               case $s$wa 0 (ipv42 `cast` ...) 0 ipv18 ipv17 0
                                               of _ { (# ipv44, ipv45 #) ->
                                               case ipv45 of _ { I# tpl3 ->
                                               case unsafeFreezeByteArray# ipv43 (ipv44 `cast` ...)
                                               of _ { (# ipv46, ipv47 #) ->
                                               (# ipv46 `cast` ...,
                                                  (Vector 0 tpl3 ipv47) `cast` ... #)
                                               }
                                               }
                                               }
                                               } } in
                                         case <=# ipv15 w3 of _ {
                                           False -> $j w3;
                                           True -> $j ipv15
                                         } } in
                                   case <=# w2 ipv20 of _ {
                                     False -> $w$j1 ipv20;
                                     True -> $w$j1 w2
                                   } } in
                             case <=# ipv17 0 of _ {
                               False -> $w$j ipv17;
                               True -> $w$j 0
                             })
                        }
                        }
                        }
                        }
                        }
                        }
                        }
                        }
                        }
                        }
                        }
                        }
                        })
                     ((DProd
                         (nt3 `cast` ...)
                         ((DProd (nt4 `cast` ...) (nt5 `cast` ...)) `cast` ...))
                      `cast` ...))
                  `cast` ...
             of nt6 { DVector ipv14 ipv15 ipv16 ipv17 ->
             case (runSTRep
                     (\ (@ s1) (s1 :: State# s1) ->
                        case scanD $fDTInt lvl27 wild7 $fNumInt_$c+ lvl13 ipv14
                        of _ { (di, n) ->
                        case di `cast` ... of nt7 { DInt ipv18 ->
                        case n of n1 { I# ipv19 ->
                        case >=# ipv19 0 of _ {
                          False -> case lvl25 ipv19 of wild14 { };
                          True ->
                            case newByteArray# (*# ipv19 8) (s1 `cast` ...)
                            of _ { (# ipv20, ipv21 #) ->
                            let {
                              nt8 :: R:Dist(,) Int (Vector Double)
                              nt8 = DProd (nt7 `cast` ...) (nt6 `cast` ...) } in
                            case ($wa1
                                    rb9
                                    ds1
                                    ds2
                                    (++
                                       $fShowComp2
                                       ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                                    (WorkCopy n1)
                                    ((\ (i :: Int) (s2 :: State# s1) ->
                                        case ((((myD lvl26 (nt8 `cast` ...)) `cast` ...) i)
                                              `cast` ...)
                                               s2
                                        of _ { (# ipv22, ipv23 #) ->
                                        case ipv23 of _ { (x, y) ->
                                        case x of _ { I# ipv24 ->
                                        case y `cast` ... of _ { Vector ipv25 ipv26 ipv27 ->
                                        case copyByteArray#
                                               ipv27
                                               (*# ipv25 8)
                                               ipv21
                                               (*# ipv24 8)
                                               (*# ipv26 8)
                                               (ipv22 `cast` ...)
                                        of s'# { __DEFAULT ->
                                        (# s'#, () #) `cast` ...
                                        }
                                        }
                                        }
                                        }
                                        })
                                     `cast` ...)
                                    (ipv20 `cast` ...))
                                 `cast` ...
                            of _ { (# ipv22, _ #) ->
                            case unsafeFreezeByteArray# ipv21 (ipv22 `cast` ...)
                            of _ { (# ipv24, ipv25 #) ->
                            (# ipv24 `cast` ..., (Vector 0 ipv19 ipv25) `cast` ... #)
                            }
                            }
                            }
                        }
                        }
                        }
                        }))
                  `cast` ...
             of _ { Vector ipv18 ipv19 ipv20 ->
             case newByteArray# (*# ipv19 8) (s `cast` ...)
             of _ { (# ipv21, ipv22 #) ->
             letrec {
               $s$wa
                 :: Int#
                    -> Int#
                    -> State# (PrimState (ST s))
                    -> (# State# (PrimState (ST s)), Int #)
               $s$wa =
                 \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
                   case >=# sc1 ipv19 of _ {
                     False ->
                       case indexDoubleArray# ipv20 (+# ipv18 sc1)
                       of wild13 { __DEFAULT ->
                       case writeDoubleArray# ipv22 sc wild13 (sc2 `cast` ...)
                       of s'# { __DEFAULT ->
                       $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                       }
                       };
                     True -> (# sc2, I# sc #)
                   }; } in
             case $s$wa 0 0 (ipv21 `cast` ...) of _ { (# ipv23, ipv24 #) ->
             case ipv24 of _ { I# tpl1 ->
             case unsafeFreezeByteArray# (ipv22 `cast` ...) ipv23
             of _ { (# ipv25, ipv26 #) ->
             case newByteArray# 4 ipv25 of _ { (# ipv27, ipv28 #) ->
             case writeIntArray# ipv28 0 0 ipv27 of s'# { __DEFAULT ->
             case unsafeFreezeByteArray# ipv28 s'# of _ { (# ipv29, ipv30 #) ->
             case newByteArray# 4 ipv29 of _ { (# ipv31, ipv32 #) ->
             case writeIntArray# ipv32 0 tpl1 ipv31 of s'#1 { __DEFAULT ->
             case unsafeFreezeByteArray# ipv32 s'#1 of _ { (# ipv33, ipv34 #) ->
             case newArrayArray# 1 (ipv33 `cast` ...)
             of _ { (# ipv35, ipv36 #) ->
             case writeByteArrayArray# ipv36 0 ipv26 ipv35
             of s'#2 { __DEFAULT ->
             case unsafeFreezeArrayArray# ipv36 s'#2
             of _ { (# ipv37, ipv38 #) ->
             (# ipv37, Vectors 1 ipv30 ipv34 ipv38 #)
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             }
             })
        }
        }
        }
        } } in
    case (imapD'
            foldSegsWithP_$dDT
            lvl54
            lvl56
            theGang
            (\ _ (x :: ((USSegd, Int), Int)) ->
               case x of _ { (x1, y) ->
               case x1 of _ { (x2, y1) ->
               case x2 of _ { USSegd ds1 ds2 ds3 rb6 rb7 rb8 ->
               case rb6 `cast` ... of _ { Vector rb9 rb10 rb11 ->
               case ds2 `cast` ... of _ { Vector ipv11 _ ipv13 ->
               case ds3 `cast` ... of _ { Vector ipv14 _ ipv16 ->
               case y1 of tpl2 { I# ipv17 ->
               case y of _ { I# ipv18 ->
               let {
                 rs :: Vector Double
                 rs =
                   case arrs of _ { Vectors ipv19 ipv20 ipv21 ipv22 ->
                   case lvl58 of _ { ByteArray ipv23 ->
                   runSTRep
                     (\ (@ s) (s :: State# s) ->
                        case newByteArray# (*# rb10 8) (s `cast` ...)
                        of _ { (# ipv24, ipv25 #) ->
                        letrec {
                          $s$wa
                            :: Int#
                               -> Int#
                               -> Double#
                               -> Int#
                               -> Int#
                               -> ByteArray#
                               -> Int#
                               -> Int#
                               -> State# s
                               -> (# State# s, Int #)
                          $s$wa =
                            \ (sc :: Int#)
                              (sc1 :: Int#)
                              (sc2 :: Double#)
                              (sc3 :: Int#)
                              (sc4 :: Int#)
                              (sc5 :: ByteArray#)
                              (sc6 :: Int#)
                              (sc7 :: Int#)
                              (sc8 :: State# s) ->
                              case sc1 of ds7 {
                                __DEFAULT ->
                                  case >=# sc7 sc6 of _ {
                                    False ->
                                      case indexDoubleArray# sc5 sc7 of wild9 { __DEFAULT ->
                                      $s$wa
                                        sc (-# ds7 1) (+## sc2 wild9) sc3 sc4 sc5 sc6 (+# sc7 1) sc8
                                      };
                                    True ->
                                      case >=# (+# sc4 1) rb10 of _ {
                                        False ->
                                          let {
                                            a5 :: Int#
                                            a5 = +# sc4 1 } in
                                          let {
                                            a6 :: Int
                                            a6 =
                                              case indexIntArray# ipv13 (+# ipv11 a5)
                                              of wild10 { __DEFAULT ->
                                              I# wild10
                                              } } in
                                          let {
                                            a7 :: Int
                                            a7 =
                                              case indexIntArray# ipv16 (+# ipv14 a5)
                                              of wild10 { __DEFAULT ->
                                              I# wild10
                                              } } in
                                          let {
                                            a8 :: Int
                                            a8 =
                                              case a7 of _ { I# i# ->
                                              case indexIntArray# ipv20 i# of wild11 { __DEFAULT ->
                                              I# wild11
                                              }
                                              } } in
                                          $s$wa1
                                            sc
                                            ds7
                                            sc2
                                            sc3
                                            a5
                                            (case a7 of _ { I# i# ->
                                             case indexByteArrayArray# ipv22 i#
                                             of wild11 { __DEFAULT ->
                                             ByteArray wild11
                                             }
                                             })
                                            (case a8 of _ { I# x3 ->
                                             case a6 of _ { I# y2 ->
                                             case indexIntArray# rb11 (+# rb9 a5)
                                             of wild12 { __DEFAULT ->
                                             I# (+# (+# x3 y2) wild12)
                                             }
                                             }
                                             })
                                            (case a8 of _ { I# x3 ->
                                             case a6 of _ { I# y2 -> I# (+# x3 y2) }
                                             })
                                            sc8;
                                        True -> (# sc8, I# sc #)
                                      }
                                  };
                                0 ->
                                  case writeDoubleArray# ipv25 sc sc2 (sc8 `cast` ...)
                                  of s'# { __DEFAULT ->
                                  case >=# sc3 rb10 of _ {
                                    False ->
                                      case indexIntArray# rb11 (+# rb9 sc3) of wild9 { __DEFAULT ->
                                      $s$wa
                                        (+# sc 1)
                                        wild9
                                        0.0
                                        (+# sc3 1)
                                        sc4
                                        sc5
                                        sc6
                                        sc7
                                        (s'# `cast` ...)
                                      };
                                    True -> (# s'# `cast` ..., I# (+# sc 1) #)
                                  }
                                  }
                              };
                          $s$wa1
                            :: Int#
                               -> Int#
                               -> Double#
                               -> Int#
                               -> Int#
                               -> ByteArray
                               -> Int
                               -> Int
                               -> State# s
                               -> (# State# s, Int #)
                          $s$wa1 =
                            \ (sc :: Int#)
                              (sc1 :: Int#)
                              (sc2 :: Double#)
                              (sc3 :: Int#)
                              (sc4 :: Int#)
                              (sc5 :: ByteArray)
                              (sc6 :: Int)
                              (sc7 :: Int)
                              (sc8 :: State# s) ->
                              case sc1 of ds7 {
                                __DEFAULT ->
                                  case sc5 of _ { ByteArray ipv26 ->
                                  case sc7 of _ { I# x3 ->
                                  case sc6 of _ { I# y2 ->
                                  case >=# x3 y2 of _ {
                                    False ->
                                      case indexDoubleArray# ipv26 x3 of wild11 { __DEFAULT ->
                                      $s$wa
                                        sc
                                        (-# ds7 1)
                                        (+## sc2 wild11)
                                        sc3
                                        sc4
                                        ipv26
                                        y2
                                        (+# x3 1)
                                        sc8
                                      };
                                    True ->
                                      case >=# (+# sc4 1) rb10 of _ {
                                        False ->
                                          let {
                                            a5 :: Int#
                                            a5 = +# sc4 1 } in
                                          let {
                                            a6 :: Int
                                            a6 =
                                              case indexIntArray# ipv13 (+# ipv11 a5)
                                              of wild12 { __DEFAULT ->
                                              I# wild12
                                              } } in
                                          let {
                                            a7 :: Int
                                            a7 =
                                              case indexIntArray# ipv16 (+# ipv14 a5)
                                              of wild12 { __DEFAULT ->
                                              I# wild12
                                              } } in
                                          let {
                                            a8 :: Int
                                            a8 =
                                              case a7 of _ { I# i# ->
                                              case indexIntArray# ipv20 i# of wild13 { __DEFAULT ->
                                              I# wild13
                                              }
                                              } } in
                                          $s$wa1
                                            sc
                                            ds7
                                            sc2
                                            sc3
                                            a5
                                            (case a7 of _ { I# i# ->
                                             case indexByteArrayArray# ipv22 i#
                                             of wild13 { __DEFAULT ->
                                             ByteArray wild13
                                             }
                                             })
                                            (case a8 of _ { I# x4 ->
                                             case a6 of _ { I# y3 ->
                                             case indexIntArray# rb11 (+# rb9 a5)
                                             of wild14 { __DEFAULT ->
                                             I# (+# (+# x4 y3) wild14)
                                             }
                                             }
                                             })
                                            (case a8 of _ { I# x4 ->
                                             case a6 of _ { I# y3 -> I# (+# x4 y3) }
                                             })
                                            sc8;
                                        True -> (# sc8, I# sc #)
                                      }
                                  }
                                  }
                                  }
                                  };
                                0 ->
                                  case writeDoubleArray# ipv25 sc sc2 (sc8 `cast` ...)
                                  of s'# { __DEFAULT ->
                                  case >=# sc3 rb10 of _ {
                                    False ->
                                      case indexIntArray# rb11 (+# rb9 sc3) of wild9 { __DEFAULT ->
                                      $s$wa1
                                        (+# sc 1)
                                        wild9
                                        0.0
                                        (+# sc3 1)
                                        sc4
                                        sc5
                                        sc6
                                        sc7
                                        (s'# `cast` ...)
                                      };
                                    True -> (# s'# `cast` ..., I# (+# sc 1) #)
                                  }
                                  }
                              }; } in
                        case >=# 0 rb10 of _ {
                          False ->
                            case indexIntArray# rb11 rb9 of wild9 { __DEFAULT ->
                            case $s$wa 0 wild9 0.0 1 (-1) ipv23 0 0 (ipv24 `cast` ...)
                            of _ { (# ipv26, ipv27 #) ->
                            case ipv27 of _ { I# tpl3 ->
                            case unsafeFreezeByteArray# ipv25 (ipv26 `cast` ...)
                            of _ { (# ipv28, ipv29 #) ->
                            (# ipv28 `cast` ..., (Vector 0 tpl3 ipv29) `cast` ... #)
                            }
                            }
                            }
                            };
                          True ->
                            case unsafeFreezeByteArray# ipv25 ipv24
                            of _ { (# ipv26, ipv27 #) ->
                            (# ipv26 `cast` ..., (Vector 0 0 ipv27) `cast` ... #)
                            }
                        }
                        })
                   }
                   } } in
               ((tpl2,
                 case rs `cast` ... of _ { Vector rb12 rb13 rb14 ->
                 let {
                   $w$j :: Int# -> Vector Double
                   $w$j =
                     \ (w2 :: Int#) ->
                       case <=# w2 0 of _ {
                         False ->
                           case <=# w2 rb13 of _ {
                             False -> (Vector rb12 rb13 rb14) `cast` ...;
                             True -> (Vector rb12 w2 rb14) `cast` ...
                           };
                         True ->
                           case <=# 0 rb13 of _ {
                             False -> (Vector rb12 rb13 rb14) `cast` ...;
                             True -> (Vector rb12 0 rb14) `cast` ...
                           }
                       } } in
                 case ipv18 of _ {
                   __DEFAULT -> $w$j 1;
                   0 -> $w$j 0
                 }
                 }),
                case rs `cast` ... of _ { Vector rb12 rb13 rb14 ->
                let {
                  $w$j :: Int# -> Vector Double
                  $w$j =
                    \ (w2 :: Int#) ->
                      let {
                        $w$j1 :: Int# -> Vector Double
                        $w$j1 =
                          \ (w3 :: Int#) ->
                            case <=# w3 rb13 of _ {
                              False ->
                                let {
                                  y2 :: Int#
                                  y2 = -# rb13 w3 } in
                                case <=# 0 y2 of _ {
                                  False -> (Vector (+# rb12 rb13) 0 rb14) `cast` ...;
                                  True -> (Vector (+# rb12 rb13) y2 rb14) `cast` ...
                                };
                              True ->
                                let {
                                  y2 :: Int#
                                  y2 = -# rb13 w3 } in
                                case <=# 0 y2 of _ {
                                  False -> (Vector (+# rb12 w3) 0 rb14) `cast` ...;
                                  True -> (Vector (+# rb12 w3) y2 rb14) `cast` ...
                                }
                            } } in
                      case <=# w2 0 of _ {
                        False -> $w$j1 w2;
                        True -> $w$j1 0
                      } } in
                case ipv18 of _ {
                  __DEFAULT -> $w$j 1;
                  0 -> $w$j 0
                }
                })
               }
               }
               }
               }
               }
               }
               }
               })
            (splitSSegdOnElemsD
               theGang
               (USSegd
                  True
                  ipv1
                  (runSTRep
                     (\ (@ s) (s :: State# s) ->
                        let {
                          $j :: Int# -> (# State# s, Vector Int #)
                          $j =
                            \ (x :: Int#) ->
                              case newByteArray# (*# x 4) (s `cast` ...)
                              of _ { (# ipv11, ipv12 #) ->
                              letrec {
                                $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                                $s$wa =
                                  \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                                    case <=# sc1 0 of _ {
                                      False ->
                                        case writeIntArray# ipv12 sc 0 (sc2 `cast` ...)
                                        of s'# { __DEFAULT ->
                                        $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                                        };
                                      True -> (# sc2, I# sc #)
                                    }; } in
                              case $s$wa 0 rb1 (ipv11 `cast` ...) of _ { (# ipv13, ipv14 #) ->
                              case ipv14 of _ { I# tpl1 ->
                              case unsafeFreezeByteArray# ipv12 (ipv13 `cast` ...)
                              of _ { (# ipv15, ipv16 #) ->
                              (# ipv15 `cast` ..., (Vector 0 tpl1 ipv16) `cast` ... #)
                              }
                              }
                              }
                              } } in
                        case <=# rb1 0 of _ {
                          False -> $j rb1;
                          True -> $j 0
                        }))
                  (wild `cast` ...)
                  ipv1
                  ipv2)))
         `cast` ...
    of _ { DProd dx dy ->
    case dy `cast` ... of wild5 { DVector l rb6 rb7 rb8 ->
    case (runSTRep
            (\ (@ s) (s :: State# s) ->
               case scanD $fDTInt lvl52 theGang $fNumInt_$c+ lvl13 l
               of _ { (di, n) ->
               case di `cast` ... of nt2 { DInt ipv11 ->
               case n of n1 { I# ipv12 ->
               case >=# ipv12 0 of _ {
                 False -> case lvl49 ipv12 of wild8 { };
                 True ->
                   case newByteArray# (*# ipv12 8) (s `cast` ...)
                   of _ { (# ipv13, ipv14 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     ds8 :: DistST s (Int, Vector Double)
                     ds8 =
                       myD
                         lvl50 ((DProd (nt2 `cast` ...) (wild5 `cast` ...)) `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s1 :: State# s) ->
                               case (((ds8 `cast` ...) i) `cast` ...) s1
                               of _ { (# ipv15, ipv16 #) ->
                               case ipv16 of _ { (x1, y) ->
                               case x1 of _ { I# ipv17 ->
                               case y `cast` ... of _ { Vector ipv18 ipv19 ipv20 ->
                               case copyByteArray#
                                      ipv20
                                      (*# ipv18 8)
                                      ipv14
                                      (*# ipv17 8)
                                      (*# ipv19 8)
                                      (ipv15 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv13 `cast` ...))
                        `cast` ...
                   of _ { (# ipv15, _ #) ->
                   case ((fixupFold
                            $fUnboxDouble plusDouble ((MVector 0 ipv12 ipv14) `cast` ...) dx)
                         `cast` ...)
                          ipv15
                   of _ { (# ipv17, _ #) ->
                   case unsafeFreezeByteArray# ipv14 (ipv17 `cast` ...)
                   of _ { (# ipv19, ipv20 #) ->
                   (# ipv19 `cast` ..., (Vector 0 ipv12 ipv20) `cast` ... #)
                   }
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv11 ipv12 ipv13 ->
    case theGang of wild6 { Gang rb9 ds1 ds2 ->
    case quotInt# ipv9 rb9 of wild7 { __DEFAULT ->
    case remInt# ipv9 rb9 of wild8 { __DEFAULT ->
    case (generateD
            $dDT2
            lvl44
            wild6
            (\ (i :: Int) ->
               case i of _ { I# x1 ->
               let {
                 $j :: Int# -> Vector Double
                 $j =
                   \ (y :: Int#) ->
                     let {
                       $j1 :: Int# -> Vector Double
                       $j1 =
                         \ (tpl3 :: Int#) ->
                           let {
                             a5 :: Int#
                             a5 = +# ipv8 y } in
                           runSTRep
                             (\ (@ s) (s :: State# s) ->
                                case newByteArray# (*# tpl3 8) (s `cast` ...)
                                of _ { (# ipv14, ipv15 #) ->
                                letrec {
                                  $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                                  $s$wa =
                                    \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                                      case >=# sc1 tpl3 of _ {
                                        False ->
                                          case indexIntArray# ipv10 (+# a5 sc1)
                                          of wild11 { __DEFAULT ->
                                          case >=# wild11 0 of _ {
                                            False -> case lvl47 wild11 ipv12 of wild13 { };
                                            True ->
                                              case <# wild11 ipv12 of _ {
                                                False -> case lvl48 wild11 ipv12 of wild14 { };
                                                True ->
                                                  case indexDoubleArray# ipv13 (+# ipv11 wild11)
                                                  of wild14 { __DEFAULT ->
                                                  case writeDoubleArray#
                                                         ipv15 sc wild14 (sc2 `cast` ...)
                                                  of s'# { __DEFAULT ->
                                                  $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                                                  }
                                                  }
                                              }
                                          }
                                          };
                                        True -> (# sc2, I# sc #)
                                      }; } in
                                case $s$wa 0 0 (ipv14 `cast` ...) of _ { (# ipv16, ipv17 #) ->
                                case ipv17 of _ { I# tpl1 ->
                                case unsafeFreezeByteArray# ipv15 (ipv16 `cast` ...)
                                of _ { (# ipv18, ipv19 #) ->
                                (# ipv18 `cast` ..., (Vector 0 tpl1 ipv19) `cast` ... #)
                                }
                                }
                                }
                                }) } in
                     case <# x1 wild8 of _ {
                       False -> $j1 wild7;
                       True -> $j1 (+# wild7 1)
                     } } in
               case <# x1 wild8 of _ {
                 False -> $j (+# (*# wild7 x1) wild8);
                 True -> $j (*# (+# wild7 1) x1)
               }
               }))
         `cast` ...
    of nt3 { DVector ipv14 ipv15 ipv16 ipv17 ->
    case (runSTRep
            (\ (@ s) (s :: State# s) ->
               case scanD $fDTInt lvl43 wild6 $fNumInt_$c+ lvl13 ipv14
               of _ { (di, n) ->
               case di `cast` ... of nt4 { DInt ipv18 ->
               case n of n1 { I# ipv19 ->
               case >=# ipv19 0 of _ {
                 False -> case lvl41 ipv19 of wild11 { };
                 True ->
                   case newByteArray# (*# ipv19 8) (s `cast` ...)
                   of _ { (# ipv20, ipv21 #) ->
                   let {
                     nt5 :: R:Dist(,) Int (Vector Double)
                     nt5 = DProd (nt4 `cast` ...) (nt3 `cast` ...) } in
                   case ($wa1
                           rb9
                           ds1
                           ds2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s1 :: State# s) ->
                               case ((((myD lvl42 (nt5 `cast` ...)) `cast` ...) i) `cast` ...) s1
                               of _ { (# ipv22, ipv23 #) ->
                               case ipv23 of _ { (x, y) ->
                               case x of _ { I# ipv24 ->
                               case y `cast` ... of _ { Vector ipv25 ipv26 ipv27 ->
                               case copyByteArray#
                                      ipv27
                                      (*# ipv25 8)
                                      ipv21
                                      (*# ipv24 8)
                                      (*# ipv26 8)
                                      (ipv22 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv20 `cast` ...))
                        `cast` ...
                   of _ { (# ipv22, _ #) ->
                   case unsafeFreezeByteArray# ipv21 (ipv22 `cast` ...)
                   of _ { (# ipv24, ipv25 #) ->
                   (# ipv24 `cast` ..., (Vector 0 ipv19 ipv25) `cast` ... #)
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of wild9 { Vector rb10 rb11 rb12 ->
    (PDouble (wild9 `cast` ...)) `cast` ...
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vdotp4
  :: Int#
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData Double
$vdotp4 =
  \ _
    (w1 :: PData (V:GHC:PArr_[::] (Double, Int)))
    (w2 :: PData (V:GHC:PArr_[::] Double)) ->
    $wldotp w1 w2

$vdotp3
  :: Int#
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData Double
$vdotp3 = $vdotp4

$vexample25 :: PR (Wrap Double)
$vexample25 = $fPRWrap $fPADouble

$vexample24 :: PR (Wrap Int)
$vexample24 = $fPRWrap $fPAInt

$vexample23 :: PR (Wrap Double, Wrap Int)
$vexample23 = $fPR(,) $vexample25 $vexample24

lvl64 :: PR (PArray (PRepr (Double, Int)))
lvl64 = $fPRPArray ($vexample23 `cast` ...)

lvl65 :: PA (Double, Int)
lvl65 = $fPA(,) ($vexample23 `cast` ...) $fPADouble $fPAInt

lvl66 :: PA (PArray (Double, Int))
lvl66 = $fPAPArray (lvl64 `cast` ...) lvl65

lvl67
  :: Int
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData Double
lvl67 =
  \ (ds :: Int)
    (v :: PData (V:GHC:PArr_[::] (Double, Int)))
    (x :: PData (V:GHC:PArr_[::] Double)) ->
    case ds of _ { I# c -> $wldotp v x }

$vdotp8
  :: V:GHC:PArr_[::] (Double, Int)
     -> V:GHC:PArr_[::] Double :-> Double
$vdotp8 =
  \ (x :: V:GHC:PArr_[::] (Double, Int)) -> Clo lvl66 $vdotp5 lvl67 x

$vdotp7
  :: Void
     -> V:GHC:PArr_[::] (Double, Int)
     -> V:GHC:PArr_[::] Double :-> Double
$vdotp7 = \ _ (arg :: V:GHC:PArr_[::] (Double, Int)) -> $vdotp8 arg

lvl68 :: PR (PArray (PRepr (Double, Int)))
lvl68 = $fPRPArray ($vexample23 `cast` ...)

lvl69 :: PA (Double, Int)
lvl69 = $fPA(,) ($vexample23 `cast` ...) $fPADouble $fPAInt

lvl70 :: PA (PArray (Double, Int))
lvl70 = $fPAPArray (lvl68 `cast` ...) lvl69

a :: Int#
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> R:PData:-> (V:GHC:PArr_[::] Double) Double
a =
  \ _ (x :: PData (V:GHC:PArr_[::] (Double, Int))) ->
    AClo lvl70 $vdotp5 lvl67 x

$vdotp2
  :: Int#
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double :-> Double)
$vdotp2 = a `cast` ...

$vdotp1
  :: Int#
     -> PData Void
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double :-> Double)
$vdotp1 =
  \ (lc :: Int#) _ (arg :: PData (V:GHC:PArr_[::] (Double, Int))) ->
    $vdotp2 lc arg

a1
  :: Int
     -> PData Void
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> PData (V:GHC:PArr_[::] Double :-> Double)
a1 =
  \ (ds :: Int) _ (x :: PData (V:GHC:PArr_[::] (Double, Int))) ->
    case ds of _ { I# c -> $vdotp2 c x }

$vdotp
  :: V:GHC:PArr_[::] (Double, Int)
     :-> (V:GHC:PArr_[::] Double :-> Double)
$vdotp = Clo $fPAVoid $vdotp7 a1 void

smvm :: SparseMatrix -> Vector -> Vector
smvm =
  \ (sm :: SparseMatrix) (v :: Vector) ->
    mapP (\ (ds :: SparseVector) -> dotp ds v) sm

$vexample14 :: forall s. State# s -> (# State# s, Vector Int #)
$vexample14 =
  \ (@ s) (s :: State# s) ->
    case newByteArray# 4 (s `cast` ...) of _ { (# ipv, ipv1 #) ->
    case writeIntArray# ipv1 0 0 ipv of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv1 s'# of _ { (# ipv2, ipv3 #) ->
    (# ipv2 `cast` ..., (Vector 0 1 ipv3) `cast` ... #)
    }
    }
    }

tpl6 :: Vector Int
tpl6 = runSTRep $vexample14

tpl7 :: Vector Int
tpl7 = runSTRep $vexample14

lvl71 :: Vector Int
lvl71 = runSTRep $vexample14

$wvsmvm
  :: Int#
     -> PData (V:GHC:PArr_[::] (Double, Int))
     -> V:GHC:PArr_[::] Double
     -> (# Int#, PData Double #)
$wvsmvm =
  \ (ww :: Int#)
    (ww1 :: PData (V:GHC:PArr_[::] (Double, Int)))
    (w :: V:GHC:PArr_[::] Double) ->
    (# ww,
       $wldotp
         ww1
         (case w of _ { PArray n xs ->
          let {
            ussegd :: UPSSegd
            ussegd =
              case (runSTRep
                      (\ (@ s) (s :: State# s) ->
                         case newByteArray# 4 (s `cast` ...) of _ { (# ipv, ipv1 #) ->
                         case writeIntArray# ipv1 0 n ipv of s'# { __DEFAULT ->
                         case unsafeFreezeByteArray# ipv1 s'# of _ { (# ipv2, ipv3 #) ->
                         (# ipv2 `cast` ..., (Vector 0 1 ipv3) `cast` ... #)
                         }
                         }
                         }))
                   `cast` ...
              of nt { Vector ipv1 ipv2 ipv3 ->
              case lvl71 `cast` ... of nt1 { Vector ipv4 ipv5 ipv6 ->
              UPSSegd
                True
                tpl6
                tpl7
                (nt `cast` ...)
                (nt1 `cast` ...)
                n
                (splitSSegdOnElemsD
                   theGang (USSegd True tpl6 tpl7 (nt `cast` ...) (nt1 `cast` ...) n))
              }
              } } in
          let {
            vsegids2 :: Vector Int
            vsegids2 =
              runSTRep
                (\ (@ s) (s :: State# s) ->
                   let {
                     $j :: Int# -> (# State# s, Vector Int #)
                     $j =
                       \ (x :: Int#) ->
                         case newByteArray# (*# x 4) (s `cast` ...)
                         of _ { (# ipv, ipv1 #) ->
                         letrec {
                           $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                           $s$wa =
                             \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                               case <=# sc1 0 of _ {
                                 False ->
                                   case writeIntArray# ipv1 sc 0 (sc2 `cast` ...)
                                   of s'# { __DEFAULT ->
                                   $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                                   };
                                 True -> (# sc2, I# sc #)
                               }; } in
                         case $s$wa 0 ww (ipv `cast` ...) of _ { (# ipv5, ipv6 #) ->
                         case ipv6 of _ { I# tpl1 ->
                         case unsafeFreezeByteArray# ipv1 (ipv5 `cast` ...)
                         of _ { (# ipv2, ipv3 #) ->
                         (# ipv2 `cast` ..., (Vector 0 tpl1 ipv3) `cast` ... #)
                         }
                         }
                         }
                         } } in
                   case <=# ww 0 of _ {
                     False -> $j ww;
                     True -> $j 0
                   }) } in
          let {
            a4 :: Dist ((USegd, Int), Int)
            a4 =
              case ussegd of _ { UPSSegd ww2 ww3 ww4 ww5 ww6 ww7 ww8 ->
              $wmkDist vsegids2 ww2 ww5 ww6
              } } in
          let {
            vsegd :: VSegd
            vsegd = UPVSegd False vsegids2 vsegids2 ussegd ussegd a4 } in
          let {
            pdatas :: PDatas (PRepr Double)
            pdatas =
              case xs `cast` ... of _ { PDouble pdata ->
              (PDoubles
                 (runSTRep
                    (\ (@ s) (s :: State# s) ->
                       case pdata `cast` ... of _ { Vector ipv ipv1 ipv2 ->
                       case newByteArray# (*# ipv1 8) (s `cast` ...)
                       of _ { (# ipv3, ipv4 #) ->
                       letrec {
                         $s$wa
                           :: Int#
                              -> Int#
                              -> State# (PrimState (ST s))
                              -> (# State# (PrimState (ST s)), Int #)
                         $s$wa =
                           \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
                             case >=# sc1 ipv1 of _ {
                               False ->
                                 case indexDoubleArray# ipv2 (+# ipv sc1) of wild3 { __DEFAULT ->
                                 case writeDoubleArray# ipv4 sc wild3 (sc2 `cast` ...)
                                 of s'# { __DEFAULT ->
                                 $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                                 }
                                 };
                               True -> (# sc2, I# sc #)
                             }; } in
                       case $s$wa 0 0 (ipv3 `cast` ...) of _ { (# ipv5, ipv6 #) ->
                       case ipv6 of _ { I# tpl1 ->
                       case unsafeFreezeByteArray# (ipv4 `cast` ...) ipv5
                       of _ { (# ipv7, ipv8 #) ->
                       case newByteArray# 4 ipv7 of _ { (# ipv9, ipv10 #) ->
                       case writeIntArray# ipv10 0 0 ipv9 of s'# { __DEFAULT ->
                       case unsafeFreezeByteArray# ipv10 s'# of _ { (# ipv11, ipv12 #) ->
                       case newByteArray# 4 ipv11 of _ { (# ipv13, ipv14 #) ->
                       case writeIntArray# ipv14 0 tpl1 ipv13 of s'#1 { __DEFAULT ->
                       case unsafeFreezeByteArray# ipv14 s'#1 of _ { (# ipv15, ipv16 #) ->
                       case newArrayArray# 1 (ipv15 `cast` ...)
                       of _ { (# ipv17, ipv18 #) ->
                       case writeByteArrayArray# ipv18 0 ipv8 ipv17 of s'#2 { __DEFAULT ->
                       case unsafeFreezeArrayArray# ipv18 s'#2
                       of _ { (# ipv19, ipv20 #) ->
                       (# ipv19, Vectors 1 ipv12 ipv16 ipv20 #)
                       }
                       }
                       }
                       }
                       }
                       }
                       }
                       }
                       }
                       }
                       }
                       }
                       }
                       })))
              `cast` ...
              } } in
          (PNested
             vsegd
             (pdatas `cast` ...)
             (unsafeDemoteToUPSegd vsegd)
             ((extractvs_delay ($fPRDouble `cast` ...) pdatas vsegd)
              `cast` ...))
          `cast` ...
          }) #)

$vsmvm6
  :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     -> V:GHC:PArr_[::] Double -> V:GHC:PArr_[::] Double
$vsmvm6 =
  \ (w :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
    (w1 :: V:GHC:PArr_[::] Double) ->
    case w of _ { PArray ww ww1 ->
    case $wvsmvm ww ww1 w1 of _ { (# ww3, ww4 #) -> PArray ww3 ww4 }
    }

$vsmvm5
  :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     -> V:GHC:PArr_[::] Double -> V:GHC:PArr_[::] Double
$vsmvm5 = $vsmvm6

lvl72 :: Int
lvl72 = checkError lvl59 lvl60 Bounds lvl61 lvl62

$wlsmvm
  :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double) -> PData (V:GHC:PArr_[::] Double)
$wlsmvm =
  \ (w :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))))
    (w1 :: PData (V:GHC:PArr_[::] Double)) ->
    case w `cast` ... of _ { PNested _ _ ipv6 ipv7 ->
    case ipv6 of wild { UPSegd rb rb1 rb2 ds1 ->
    case rb `cast` ... of wild1 { Vector rb3 rb4 rb5 ->
    case ipv7 `cast` ... of _ { PNested vsegd xs segd flat ->
    case vsegd of wild3 { UPVSegd ds2 ds3 ds4 ds5 ds6 ds7 ->
    case ds3 of _ { __DEFAULT ->
    let {
      a4 :: PData Double
      a4 =
        $wldotp
          ((PNested wild3 xs segd flat) `cast` ...)
          (case w1 `cast` ... of _ { PNested vsegd1 xs1 _ _ ->
           let {
             vsegd' :: VSegd
             vsegd' = updateVSegs (replicate_s $fEltInt wild) vsegd1 } in
           (PNested
              vsegd'
              xs1
              (unsafeDemoteToUPSegd vsegd')
              ((extractvs_delay ($fPRDouble `cast` ...) (xs1 `cast` ...) vsegd')
               `cast` ...))
           `cast` ...
           }) } in
    (PNested
       (let {
          vsegids2 :: Vector Int
          vsegids2 =
            let {
              a5 :: Int#
              a5 = -# rb4 1 } in
            runSTRep
              (\ (@ s) (s :: State# s) ->
                 let {
                   $j :: Int# -> (# State# s, Vector Int #)
                   $j =
                     \ (x :: Int#) ->
                       case newByteArray# (*# x 4) (s `cast` ...)
                       of _ { (# ipv, ipv1 #) ->
                       letrec {
                         $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                         $s$wa =
                           \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                             case <=# sc1 a5 of _ {
                               False -> (# sc2, I# sc #);
                               True ->
                                 case writeIntArray# ipv1 sc sc1 (sc2 `cast` ...)
                                 of s'# { __DEFAULT ->
                                 $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                                 }
                             }; } in
                       case $s$wa 0 0 (ipv `cast` ...) of _ { (# ipv2, ipv3 #) ->
                       case ipv3 of _ { I# tpl2 ->
                       case unsafeFreezeByteArray# ipv1 (ipv2 `cast` ...)
                       of _ { (# ipv8, ipv9 #) ->
                       (# ipv8 `cast` ..., (Vector 0 tpl2 ipv9) `cast` ... #)
                       }
                       }
                       }
                       } } in
                 case ># 0 a5 of _ {
                   False ->
                     let {
                       a6 :: Int#
                       a6 = +# a5 1 } in
                     case ># a6 0 of _ {
                       False -> case lvl72 of wild6 { };
                       True -> $j a6
                     };
                   True -> $j 0
                 }) } in
        let {
          tpl5 :: Vector Int
          tpl5 =
            runSTRep
              (\ (@ s) (s :: State# s) ->
                 let {
                   $j :: Int# -> (# State# s, Vector Int #)
                   $j =
                     \ (x :: Int#) ->
                       case newByteArray# (*# x 4) (s `cast` ...)
                       of _ { (# ipv, ipv1 #) ->
                       letrec {
                         $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                         $s$wa =
                           \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                             case <=# sc1 0 of _ {
                               False ->
                                 case writeIntArray# ipv1 sc 0 (sc2 `cast` ...)
                                 of s'# { __DEFAULT ->
                                 $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                                 };
                               True -> (# sc2, I# sc #)
                             }; } in
                       case $s$wa 0 rb4 (ipv `cast` ...) of _ { (# ipv2, ipv3 #) ->
                       case ipv3 of _ { I# tpl2 ->
                       case unsafeFreezeByteArray# ipv1 (ipv2 `cast` ...)
                       of _ { (# ipv8, ipv9 #) ->
                       (# ipv8 `cast` ..., (Vector 0 tpl2 ipv9) `cast` ... #)
                       }
                       }
                       }
                       } } in
                 case <=# rb4 0 of _ {
                   False -> $j rb4;
                   True -> $j 0
                 }) } in
        let {
          a5 :: Dist ((USSegd, Int), Int)
          a5 =
            splitSSegdOnElemsD
              theGang (USSegd True rb1 tpl5 (wild1 `cast` ...) rb1 rb2) } in
        let {
          upssegd :: UPSSegd
          upssegd = UPSSegd True rb1 tpl5 (wild1 `cast` ...) rb1 rb2 a5 } in
        UPVSegd
          True
          vsegids2
          vsegids2
          upssegd
          upssegd
          ($wmkDist vsegids2 True (wild1 `cast` ...) rb1))
       ($fPRDouble_$csingletondPR a4)
       wild
       a4)
    `cast` ...
    }
    }
    }
    }
    }
    }

$vsmvm4
  :: Int#
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData (V:GHC:PArr_[::] Double)
$vsmvm4 =
  \ _
    (w1 :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))))
    (w2 :: PData (V:GHC:PArr_[::] Double)) ->
    $wlsmvm w1 w2

$vsmvm3
  :: Int#
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData (V:GHC:PArr_[::] Double)
$vsmvm3 = $vsmvm4

lvl73 :: PR (PArray (PRepr (Double, Int)))
lvl73 = $fPRPArray ($vexample23 `cast` ...)

lvl74 :: PR (PArray (PRepr (PArray (Double, Int))))
lvl74 = $fPRPArray (lvl73 `cast` ...)

lvl75 :: PR (PArray (PRepr (Double, Int)))
lvl75 = $fPRPArray ($vexample23 `cast` ...)

lvl76 :: PA (Double, Int)
lvl76 = $fPA(,) ($vexample23 `cast` ...) $fPADouble $fPAInt

lvl77 :: PA (PArray (Double, Int))
lvl77 = $fPAPArray (lvl75 `cast` ...) lvl76

lvl78 :: PA (PArray (V:GHC:PArr_[::] (Double, Int)))
lvl78 = $fPAPArray (lvl74 `cast` ...) lvl77

lvl79
  :: Int
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double)
     -> PData (V:GHC:PArr_[::] Double)
lvl79 =
  \ (ds :: Int)
    (v :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))))
    (x :: PData (V:GHC:PArr_[::] Double)) ->
    case ds of _ { I# c -> $wlsmvm v x }

$vsmvm8
  :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     -> V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double
$vsmvm8 =
  \ (x :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))) ->
    Clo lvl78 $vsmvm5 lvl79 x

$vsmvm7
  :: Void
     -> V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     -> V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double
$vsmvm7 =
  \ _ (arg :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))) ->
    $vsmvm8 arg

lvl80 :: PR (PArray (PRepr (Double, Int)))
lvl80 = $fPRPArray ($vexample23 `cast` ...)

lvl81 :: PR (PArray (PRepr (PArray (Double, Int))))
lvl81 = $fPRPArray (lvl80 `cast` ...)

lvl82 :: PR (PArray (PRepr (Double, Int)))
lvl82 = $fPRPArray ($vexample23 `cast` ...)

lvl83 :: PA (Double, Int)
lvl83 = $fPA(,) ($vexample23 `cast` ...) $fPADouble $fPAInt

lvl84 :: PA (PArray (Double, Int))
lvl84 = $fPAPArray (lvl82 `cast` ...) lvl83

lvl85 :: PA (PArray (V:GHC:PArr_[::] (Double, Int)))
lvl85 = $fPAPArray (lvl81 `cast` ...) lvl84

a2
  :: Int#
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> R:PData:-> (V:GHC:PArr_[::] Double) (V:GHC:PArr_[::] Double)
a2 =
  \ _
    (x :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))) ->
    AClo lvl85 $vsmvm5 lvl79 x

$vsmvm2
  :: Int#
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double)
$vsmvm2 = a2 `cast` ...

$vsmvm1
  :: Int#
     -> PData Void
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double)
$vsmvm1 =
  \ (lc :: Int#)
    _
    (arg :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))) ->
    $vsmvm2 lc arg

a3
  :: Int
     -> PData Void
     -> PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))
     -> PData (V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double)
a3 =
  \ (ds :: Int)
    _
    (x :: PData (V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int)))) ->
    case ds of _ { I# c -> $vsmvm2 c x }

$vsmvm
  :: V:GHC:PArr_[::] (V:GHC:PArr_[::] (Double, Int))
     :-> (V:GHC:PArr_[::] Double :-> V:GHC:PArr_[::] Double)
$vsmvm = Clo $fPAVoid $vsmvm7 a3 void

len :: Int
len = I# 1

lvl86 :: DT (Int, Vector Double)
lvl86 = $fDT(,) $fDTInt $dDT2

lvl87 :: What
lvl87 = What lvl11

lvl88 :: [Char]
lvl88 = unpackCString# "replicateUP/replicate"

lvl89 :: What
lvl89 = What lvl88

lvl90 :: Int -> Int -> Vector Double
lvl90 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 2.1 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl91 :: Dist Int
lvl91 = splitLenD theGang len

lvl92 :: DT (Int, Vector Double)
lvl92 = $fDT(,) $fDTInt $dDT2

lvl93 :: What
lvl93 = What lvl11

lvl94 :: What
lvl94 = What lvl88

lvl95 :: Int -> Int -> Vector Double
lvl95 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 3.2 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl96 :: DT (Int, Vector Double)
lvl96 = $fDT(,) $fDTInt $dDT2

lvl97 :: What
lvl97 = What lvl11

lvl98 :: What
lvl98 = What lvl88

lvl99 :: Int -> Int -> Vector Double
lvl99 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 3.1 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

$vexample22 :: forall s. State# s -> (# State# s, Vectors Double #)
$vexample22 =
  \ (@ s) (s :: State# s) ->
    case (imapD' $fDTInt $dDT2 lvl98 theGang lvl99 lvl91) `cast` ...
    of nt { DVector ipv ipv1 ipv2 ipv3 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl97 theGang $fNumInt_$c+ lvl13 ipv
               of _ { (di, n) ->
               case di `cast` ... of nt1 { DInt ipv4 ->
               case n of n1 { I# ipv5 ->
               case >=# ipv5 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv5 8) (s1 `cast` ...)
                   of _ { (# ipv6, ipv7 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt2 :: R:Dist(,) Int (Vector Double)
                     nt2 = DProd (nt1 `cast` ...) (nt `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl96 (nt2 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv10, ipv11 #) ->
                               case ipv11 of _ { (x, y) ->
                               case x of _ { I# ipv12 ->
                               case y `cast` ... of _ { Vector ipv8 ipv9 ipv13 ->
                               case copyByteArray#
                                      ipv13
                                      (*# ipv8 8)
                                      ipv7
                                      (*# ipv12 8)
                                      (*# ipv9 8)
                                      (ipv10 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv6 `cast` ...))
                        `cast` ...
                   of _ { (# ipv10, _ #) ->
                   case unsafeFreezeByteArray# ipv7 (ipv10 `cast` ...)
                   of _ { (# ipv8, ipv9 #) ->
                   (# ipv8 `cast` ..., (Vector 0 ipv5 ipv9) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv4 ipv5 ipv6 ->
    case (imapD' $fDTInt $dDT2 lvl94 theGang lvl95 lvl91) `cast` ...
    of nt2 { DVector ipv7 ipv8 ipv9 ipv10 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl93 theGang $fNumInt_$c+ lvl13 ipv7
               of _ { (di, n) ->
               case di `cast` ... of nt3 { DInt ipv11 ->
               case n of n1 { I# ipv12 ->
               case >=# ipv12 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv12)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv12 8) (s1 `cast` ...)
                   of _ { (# ipv13, ipv14 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt4 :: R:Dist(,) Int (Vector Double)
                     nt4 = DProd (nt3 `cast` ...) (nt2 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl92 (nt4 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv15, ipv16 #) ->
                               case ipv16 of _ { (x, y) ->
                               case x of _ { I# ipv17 ->
                               case y `cast` ... of _ { Vector ipv18 ipv19 ipv20 ->
                               case copyByteArray#
                                      ipv20
                                      (*# ipv18 8)
                                      ipv14
                                      (*# ipv17 8)
                                      (*# ipv19 8)
                                      (ipv15 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv13 `cast` ...))
                        `cast` ...
                   of _ { (# ipv15, _ #) ->
                   case unsafeFreezeByteArray# ipv14 (ipv15 `cast` ...)
                   of _ { (# ipv17, ipv18 #) ->
                   (# ipv17 `cast` ..., (Vector 0 ipv12 ipv18) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv11 ipv12 ipv13 ->
    case (imapD' $fDTInt $dDT2 lvl89 theGang lvl90 lvl91) `cast` ...
    of nt4 { DVector ipv14 ipv15 ipv16 ipv17 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl87 theGang $fNumInt_$c+ lvl13 ipv14
               of _ { (di, n) ->
               case di `cast` ... of nt5 { DInt ipv18 ->
               case n of n1 { I# ipv19 ->
               case >=# ipv19 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv19)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv19 8) (s1 `cast` ...)
                   of _ { (# ipv20, ipv21 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt6 :: R:Dist(,) Int (Vector Double)
                     nt6 = DProd (nt5 `cast` ...) (nt4 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl86 (nt6 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv22, ipv23 #) ->
                               case ipv23 of _ { (x, y) ->
                               case x of _ { I# ipv24 ->
                               case y `cast` ... of _ { Vector ipv25 ipv26 ipv27 ->
                               case copyByteArray#
                                      ipv27
                                      (*# ipv25 8)
                                      ipv21
                                      (*# ipv24 8)
                                      (*# ipv26 8)
                                      (ipv22 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv20 `cast` ...))
                        `cast` ...
                   of _ { (# ipv22, _ #) ->
                   case unsafeFreezeByteArray# ipv21 (ipv22 `cast` ...)
                   of _ { (# ipv24, ipv25 #) ->
                   (# ipv24 `cast` ..., (Vector 0 ipv19 ipv25) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv18 ipv19 ipv20 ->
    case newByteArray# (*# (+# ipv5 (+# ipv12 ipv19)) 8) (s `cast` ...)
    of _ { (# ipv21, ipv22 #) ->
    letrec {
      $s$wa
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv19 of _ {
            False ->
              case indexDoubleArray# ipv20 (+# ipv18 sc1) of wild { __DEFAULT ->
              case writeDoubleArray# ipv22 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> (# sc2, I# sc #)
          }; } in
    letrec {
      $s$wa1
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa1 =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv12 of _ {
            False ->
              case indexDoubleArray# ipv13 (+# ipv11 sc1) of wild { __DEFAULT ->
              case writeDoubleArray# ipv22 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa1 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> $s$wa sc 0 sc2
          }; } in
    letrec {
      $s$wa2
        :: Int#
           -> State# (PrimState (ST s))
           -> Int#
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa2 =
        \ (sc :: Int#) (sc1 :: State# (PrimState (ST s))) (sc2 :: Int#) ->
          case >=# sc2 ipv5 of _ {
            False ->
              case indexDoubleArray# ipv6 (+# ipv4 sc2) of wild { __DEFAULT ->
              case writeDoubleArray# ipv22 sc wild (sc1 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa2 (+# sc 1) (s'# `cast` ...) (+# sc2 1)
              }
              };
            True -> $s$wa1 sc 0 sc1
          }; } in
    case $s$wa2 0 (ipv21 `cast` ...) 0 of _ { (# ipv23, ipv24 #) ->
    case ipv24 of _ { I# tpl1 ->
    case unsafeFreezeByteArray# (ipv22 `cast` ...) ipv23
    of _ { (# ipv25, ipv26 #) ->
    case newByteArray# 4 ipv25 of _ { (# ipv27, ipv28 #) ->
    case writeIntArray# ipv28 0 0 ipv27 of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv28 s'# of _ { (# ipv29, ipv30 #) ->
    case newByteArray# 4 ipv29 of _ { (# ipv31, ipv32 #) ->
    case writeIntArray# ipv32 0 tpl1 ipv31 of s'#1 { __DEFAULT ->
    case unsafeFreezeByteArray# ipv32 s'#1 of _ { (# ipv33, ipv34 #) ->
    case newArrayArray# 1 (ipv33 `cast` ...)
    of _ { (# ipv35, ipv36 #) ->
    case writeByteArrayArray# ipv36 0 ipv26 ipv35
    of s'#2 { __DEFAULT ->
    case unsafeFreezeArrayArray# ipv36 s'#2
    of _ { (# ipv37, ipv38 #) ->
    (# ipv37, Vectors 1 ipv30 ipv34 ipv38 #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vexample21 :: Arrays Double
$vexample21 = runSTRep $vexample22

$vexample20 :: R:PDatasDouble
$vexample20 = PDoubles $vexample21

lvl100 :: DT (Int, Vector Int)
lvl100 = $fDT(,) $fDTInt $dDT1

lvl101 :: What
lvl101 = What lvl11

lvl102 :: What
lvl102 = What lvl88

lvl103 :: Int -> Int -> Vector Int
lvl103 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Int #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 4) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeIntArray# ipv2 sc 5 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl104 :: DT (Int, Vector Int)
lvl104 = $fDT(,) $fDTInt $dDT1

lvl105 :: What
lvl105 = What lvl11

lvl106 :: What
lvl106 = What lvl88

lvl107 :: Int -> Int -> Vector Int
lvl107 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Int #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 4) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeIntArray# ipv2 sc 2 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl108 :: DT (Int, Vector Int)
lvl108 = $fDT(,) $fDTInt $dDT1

lvl109 :: What
lvl109 = What lvl11

lvl110 :: What
lvl110 = What lvl88

lvl111 :: Int -> Int -> Vector Int
lvl111 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Int #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 4) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeIntArray# ipv2 sc 1 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

$vexample19 :: forall s. State# s -> (# State# s, Vectors Int #)
$vexample19 =
  \ (@ s) (s :: State# s) ->
    case (imapD' $fDTInt $dDT1 lvl110 theGang lvl111 lvl91) `cast` ...
    of nt { DVector ipv ipv1 ipv2 ipv3 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl109 theGang $fNumInt_$c+ lvl13 ipv
               of _ { (di, n) ->
               case di `cast` ... of nt1 { DInt ipv4 ->
               case n of n1 { I# ipv5 ->
               case >=# ipv5 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv5 4) (s1 `cast` ...)
                   of _ { (# ipv6, ipv7 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt2 :: R:Dist(,) Int (Vector Int)
                     nt2 = DProd (nt1 `cast` ...) (nt `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl108 (nt2 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv10, ipv11 #) ->
                               case ipv11 of _ { (x, y) ->
                               case x of _ { I# ipv12 ->
                               case y `cast` ... of _ { Vector ipv8 ipv9 ipv13 ->
                               case copyByteArray#
                                      ipv13
                                      (*# ipv8 4)
                                      ipv7
                                      (*# ipv12 4)
                                      (*# ipv9 4)
                                      (ipv10 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv6 `cast` ...))
                        `cast` ...
                   of _ { (# ipv10, _ #) ->
                   case unsafeFreezeByteArray# ipv7 (ipv10 `cast` ...)
                   of _ { (# ipv8, ipv9 #) ->
                   (# ipv8 `cast` ..., (Vector 0 ipv5 ipv9) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv4 ipv5 ipv6 ->
    case (imapD' $fDTInt $dDT1 lvl106 theGang lvl107 lvl91) `cast` ...
    of nt2 { DVector ipv7 ipv8 ipv9 ipv10 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl105 theGang $fNumInt_$c+ lvl13 ipv7
               of _ { (di, n) ->
               case di `cast` ... of nt3 { DInt ipv11 ->
               case n of n1 { I# ipv12 ->
               case >=# ipv12 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv12)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv12 4) (s1 `cast` ...)
                   of _ { (# ipv13, ipv14 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt4 :: R:Dist(,) Int (Vector Int)
                     nt4 = DProd (nt3 `cast` ...) (nt2 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl104 (nt4 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv15, ipv16 #) ->
                               case ipv16 of _ { (x, y) ->
                               case x of _ { I# ipv17 ->
                               case y `cast` ... of _ { Vector ipv18 ipv19 ipv20 ->
                               case copyByteArray#
                                      ipv20
                                      (*# ipv18 4)
                                      ipv14
                                      (*# ipv17 4)
                                      (*# ipv19 4)
                                      (ipv15 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv13 `cast` ...))
                        `cast` ...
                   of _ { (# ipv15, _ #) ->
                   case unsafeFreezeByteArray# ipv14 (ipv15 `cast` ...)
                   of _ { (# ipv17, ipv18 #) ->
                   (# ipv17 `cast` ..., (Vector 0 ipv12 ipv18) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv11 ipv12 ipv13 ->
    case (imapD' $fDTInt $dDT1 lvl102 theGang lvl103 lvl91) `cast` ...
    of nt4 { DVector ipv14 ipv15 ipv16 ipv17 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl101 theGang $fNumInt_$c+ lvl13 ipv14
               of _ { (di, n) ->
               case di `cast` ... of nt5 { DInt ipv18 ->
               case n of n1 { I# ipv19 ->
               case >=# ipv19 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv19)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv19 4) (s1 `cast` ...)
                   of _ { (# ipv20, ipv21 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt6 :: R:Dist(,) Int (Vector Int)
                     nt6 = DProd (nt5 `cast` ...) (nt4 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl100 (nt6 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv22, ipv23 #) ->
                               case ipv23 of _ { (x, y) ->
                               case x of _ { I# ipv24 ->
                               case y `cast` ... of _ { Vector ipv25 ipv26 ipv27 ->
                               case copyByteArray#
                                      ipv27
                                      (*# ipv25 4)
                                      ipv21
                                      (*# ipv24 4)
                                      (*# ipv26 4)
                                      (ipv22 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv20 `cast` ...))
                        `cast` ...
                   of _ { (# ipv22, _ #) ->
                   case unsafeFreezeByteArray# ipv21 (ipv22 `cast` ...)
                   of _ { (# ipv24, ipv25 #) ->
                   (# ipv24 `cast` ..., (Vector 0 ipv19 ipv25) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv18 ipv19 ipv20 ->
    case newByteArray# (*# (+# ipv5 (+# ipv12 ipv19)) 4) (s `cast` ...)
    of _ { (# ipv21, ipv22 #) ->
    letrec {
      $s$wa
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv19 of _ {
            False ->
              case indexIntArray# ipv20 (+# ipv18 sc1) of wild { __DEFAULT ->
              case writeIntArray# ipv22 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> (# sc2, I# sc #)
          }; } in
    letrec {
      $s$wa1
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa1 =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv12 of _ {
            False ->
              case indexIntArray# ipv13 (+# ipv11 sc1) of wild { __DEFAULT ->
              case writeIntArray# ipv22 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa1 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> $s$wa sc 0 sc2
          }; } in
    letrec {
      $s$wa2
        :: Int#
           -> State# (PrimState (ST s))
           -> Int#
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa2 =
        \ (sc :: Int#) (sc1 :: State# (PrimState (ST s))) (sc2 :: Int#) ->
          case >=# sc2 ipv5 of _ {
            False ->
              case indexIntArray# ipv6 (+# ipv4 sc2) of wild { __DEFAULT ->
              case writeIntArray# ipv22 sc wild (sc1 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa2 (+# sc 1) (s'# `cast` ...) (+# sc2 1)
              }
              };
            True -> $s$wa1 sc 0 sc1
          }; } in
    case $s$wa2 0 (ipv21 `cast` ...) 0 of _ { (# ipv23, ipv24 #) ->
    case ipv24 of _ { I# tpl1 ->
    case unsafeFreezeByteArray# (ipv22 `cast` ...) ipv23
    of _ { (# ipv25, ipv26 #) ->
    case newByteArray# 4 ipv25 of _ { (# ipv27, ipv28 #) ->
    case writeIntArray# ipv28 0 0 ipv27 of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv28 s'# of _ { (# ipv29, ipv30 #) ->
    case newByteArray# 4 ipv29 of _ { (# ipv31, ipv32 #) ->
    case writeIntArray# ipv32 0 tpl1 ipv31 of s'#1 { __DEFAULT ->
    case unsafeFreezeByteArray# ipv32 s'#1 of _ { (# ipv33, ipv34 #) ->
    case newArrayArray# 1 (ipv33 `cast` ...)
    of _ { (# ipv35, ipv36 #) ->
    case writeByteArrayArray# ipv36 0 ipv26 ipv35
    of s'#2 { __DEFAULT ->
    case unsafeFreezeArrayArray# ipv36 s'#2
    of _ { (# ipv37, ipv38 #) ->
    (# ipv37, Vectors 1 ipv30 ipv34 ipv38 #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vexample18 :: Arrays Int
$vexample18 = runSTRep $vexample19

$vexample17 :: R:PDatasInt
$vexample17 = PInts $vexample18

$vexample13 :: forall s. State# s -> (# State# s, Vector Int #)
$vexample13 =
  \ (@ s) (s :: State# s) ->
    case newByteArray# 4 (s `cast` ...) of _ { (# ipv, ipv1 #) ->
    letrec {
      $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
      $s$wa =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
          case <=# sc1 0 of _ {
            False ->
              case writeIntArray# ipv1 sc 0 (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
              };
            True -> (# sc2, I# sc #)
          }; } in
    case $s$wa 0 1 (ipv `cast` ...) of _ { (# ipv5, ipv6 #) ->
    case ipv6 of _ { I# tpl1 ->
    case unsafeFreezeByteArray# ipv1 (ipv5 `cast` ...)
    of _ { (# ipv2, ipv3 #) ->
    (# ipv2 `cast` ..., (Vector 0 tpl1 ipv3) `cast` ... #)
    }
    }
    }
    }

$vexample_vsegids2 :: Vector Int
$vexample_vsegids2 = runSTRep $vexample13

$vexample_tpl6 :: Vector Int
$vexample_tpl6 = runSTRep $vexample14

$vexample_tpl7 :: Vector Int
$vexample_tpl7 = runSTRep $vexample14

$vexample15 :: forall s. State# s -> (# State# s, Vector Int #)
$vexample15 =
  \ (@ s) (s :: State# s) ->
    case newByteArray# 4 (s `cast` ...) of _ { (# ipv, ipv1 #) ->
    case writeIntArray# ipv1 0 3 ipv of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv1 s'# of _ { (# ipv2, ipv3 #) ->
    (# ipv2 `cast` ..., (Vector 0 1 ipv3) `cast` ... #)
    }
    }
    }

$vexample_ussegd :: UPSSegd
$vexample_ussegd =
  case (runSTRep $vexample15) `cast` ...
  of nt { Vector ipv1 ipv2 ipv3 ->
  case (runSTRep $vexample14) `cast` ...
  of nt1 { Vector ipv4 ipv5 ipv6 ->
  UPSSegd
    True
    $vexample_tpl6
    $vexample_tpl7
    (nt `cast` ...)
    (nt1 `cast` ...)
    3
    (splitSSegdOnElemsD
       theGang
       (USSegd
          True
          $vexample_tpl6
          $vexample_tpl7
          (nt `cast` ...)
          (nt1 `cast` ...)
          3))
  }
  }

$vexample12 :: Dist ((USegd, Int), Int)
$vexample12 =
  case $vexample_ussegd of _ { UPSSegd ww ww1 ww2 ww3 ww4 ww5 ww6 ->
  $wmkDist $vexample_vsegids2 ww ww3 ww4
  }

$vexample_vsegd :: VSegd
$vexample_vsegd =
  UPVSegd
    False
    $vexample_vsegids2
    $vexample_vsegids2
    $vexample_ussegd
    $vexample_ussegd
    $vexample12

lvl112 :: DT (Int, Vector Double)
lvl112 = $fDT(,) $fDTInt $dDT2

lvl113 :: What
lvl113 = What lvl11

lvl114 :: What
lvl114 = What lvl88

lvl115 :: Int -> Int -> Vector Double
lvl115 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 1.4 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl116 :: DT (Int, Vector Double)
lvl116 = $fDT(,) $fDTInt $dDT2

lvl117 :: What
lvl117 = What lvl11

lvl118 :: What
lvl118 = What lvl88

lvl119 :: Int -> Int -> Vector Double
lvl119 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 5.5 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

$vexample37 :: forall s. State# s -> (# State# s, Vectors Double #)
$vexample37 =
  \ (@ s) (s :: State# s) ->
    case (imapD' $fDTInt $dDT2 lvl118 theGang lvl119 lvl91) `cast` ...
    of nt { DVector ipv ipv1 ipv2 ipv3 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl117 theGang $fNumInt_$c+ lvl13 ipv
               of _ { (di, n) ->
               case di `cast` ... of nt1 { DInt ipv4 ->
               case n of n1 { I# ipv5 ->
               case >=# ipv5 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv5 8) (s1 `cast` ...)
                   of _ { (# ipv6, ipv7 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt2 :: R:Dist(,) Int (Vector Double)
                     nt2 = DProd (nt1 `cast` ...) (nt `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl116 (nt2 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv10, ipv11 #) ->
                               case ipv11 of _ { (x, y) ->
                               case x of _ { I# ipv12 ->
                               case y `cast` ... of _ { Vector ipv8 ipv9 ipv13 ->
                               case copyByteArray#
                                      ipv13
                                      (*# ipv8 8)
                                      ipv7
                                      (*# ipv12 8)
                                      (*# ipv9 8)
                                      (ipv10 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv6 `cast` ...))
                        `cast` ...
                   of _ { (# ipv10, _ #) ->
                   case unsafeFreezeByteArray# ipv7 (ipv10 `cast` ...)
                   of _ { (# ipv8, ipv9 #) ->
                   (# ipv8 `cast` ..., (Vector 0 ipv5 ipv9) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv4 ipv5 ipv6 ->
    case (imapD' $fDTInt $dDT2 lvl114 theGang lvl115 lvl91) `cast` ...
    of nt2 { DVector ipv7 ipv8 ipv9 ipv10 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl113 theGang $fNumInt_$c+ lvl13 ipv7
               of _ { (di, n) ->
               case di `cast` ... of nt3 { DInt ipv11 ->
               case n of n1 { I# ipv12 ->
               case >=# ipv12 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv12)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv12 8) (s1 `cast` ...)
                   of _ { (# ipv13, ipv14 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt4 :: R:Dist(,) Int (Vector Double)
                     nt4 = DProd (nt3 `cast` ...) (nt2 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl112 (nt4 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv15, ipv16 #) ->
                               case ipv16 of _ { (x, y) ->
                               case x of _ { I# ipv17 ->
                               case y `cast` ... of _ { Vector ipv18 ipv19 ipv20 ->
                               case copyByteArray#
                                      ipv20
                                      (*# ipv18 8)
                                      ipv14
                                      (*# ipv17 8)
                                      (*# ipv19 8)
                                      (ipv15 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv13 `cast` ...))
                        `cast` ...
                   of _ { (# ipv15, _ #) ->
                   case unsafeFreezeByteArray# ipv14 (ipv15 `cast` ...)
                   of _ { (# ipv17, ipv18 #) ->
                   (# ipv17 `cast` ..., (Vector 0 ipv12 ipv18) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv11 ipv12 ipv13 ->
    case newByteArray# (*# (+# ipv5 ipv12) 8) (s `cast` ...)
    of _ { (# ipv14, ipv15 #) ->
    letrec {
      $s$wa
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv12 of _ {
            False ->
              case indexDoubleArray# ipv13 (+# ipv11 sc1) of wild { __DEFAULT ->
              case writeDoubleArray# ipv15 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> (# sc2, I# sc #)
          }; } in
    letrec {
      $s$wa1
        :: Int#
           -> State# (PrimState (ST s))
           -> Int#
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa1 =
        \ (sc :: Int#) (sc1 :: State# (PrimState (ST s))) (sc2 :: Int#) ->
          case >=# sc2 ipv5 of _ {
            False ->
              case indexDoubleArray# ipv6 (+# ipv4 sc2) of wild { __DEFAULT ->
              case writeDoubleArray# ipv15 sc wild (sc1 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa1 (+# sc 1) (s'# `cast` ...) (+# sc2 1)
              }
              };
            True -> $s$wa sc 0 sc1
          }; } in
    case $s$wa1 0 (ipv14 `cast` ...) 0 of _ { (# ipv16, ipv17 #) ->
    case ipv17 of _ { I# tpl1 ->
    case unsafeFreezeByteArray# (ipv15 `cast` ...) ipv16
    of _ { (# ipv18, ipv19 #) ->
    case newByteArray# 4 ipv18 of _ { (# ipv20, ipv21 #) ->
    case writeIntArray# ipv21 0 0 ipv20 of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv21 s'# of _ { (# ipv22, ipv23 #) ->
    case newByteArray# 4 ipv22 of _ { (# ipv24, ipv25 #) ->
    case writeIntArray# ipv25 0 tpl1 ipv24 of s'#1 { __DEFAULT ->
    case unsafeFreezeByteArray# ipv25 s'#1 of _ { (# ipv26, ipv27 #) ->
    case newArrayArray# 1 (ipv26 `cast` ...)
    of _ { (# ipv28, ipv29 #) ->
    case writeByteArrayArray# ipv29 0 ipv19 ipv28
    of s'#2 { __DEFAULT ->
    case unsafeFreezeArrayArray# ipv29 s'#2
    of _ { (# ipv30, ipv31 #) ->
    (# ipv30, Vectors 1 ipv23 ipv27 ipv31 #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vexample36 :: Arrays Double
$vexample36 = runSTRep $vexample37

$vexample35 :: R:PDatasDouble
$vexample35 = PDoubles $vexample36

lvl120 :: DT (Int, Vector Int)
lvl120 = $fDT(,) $fDTInt $dDT1

lvl121 :: What
lvl121 = What lvl11

lvl122 :: What
lvl122 = What lvl88

lvl123 :: Int -> Int -> Vector Int
lvl123 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Int #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 4) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeIntArray# ipv2 sc 2 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl124 :: DT (Int, Vector Int)
lvl124 = $fDT(,) $fDTInt $dDT1

lvl125 :: What
lvl125 = What lvl11

lvl126 :: What
lvl126 = What lvl88

lvl127 :: Int -> Int -> Vector Int
lvl127 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Int #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 4) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeIntArray# ipv2 sc 0 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

$vexample34 :: forall s. State# s -> (# State# s, Vectors Int #)
$vexample34 =
  \ (@ s) (s :: State# s) ->
    case (imapD' $fDTInt $dDT1 lvl126 theGang lvl127 lvl91) `cast` ...
    of nt { DVector ipv ipv1 ipv2 ipv3 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl125 theGang $fNumInt_$c+ lvl13 ipv
               of _ { (di, n) ->
               case di `cast` ... of nt1 { DInt ipv4 ->
               case n of n1 { I# ipv5 ->
               case >=# ipv5 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv5 4) (s1 `cast` ...)
                   of _ { (# ipv6, ipv7 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt2 :: R:Dist(,) Int (Vector Int)
                     nt2 = DProd (nt1 `cast` ...) (nt `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl124 (nt2 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv10, ipv11 #) ->
                               case ipv11 of _ { (x, y) ->
                               case x of _ { I# ipv12 ->
                               case y `cast` ... of _ { Vector ipv8 ipv9 ipv13 ->
                               case copyByteArray#
                                      ipv13
                                      (*# ipv8 4)
                                      ipv7
                                      (*# ipv12 4)
                                      (*# ipv9 4)
                                      (ipv10 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv6 `cast` ...))
                        `cast` ...
                   of _ { (# ipv10, _ #) ->
                   case unsafeFreezeByteArray# ipv7 (ipv10 `cast` ...)
                   of _ { (# ipv8, ipv9 #) ->
                   (# ipv8 `cast` ..., (Vector 0 ipv5 ipv9) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv4 ipv5 ipv6 ->
    case (imapD' $fDTInt $dDT1 lvl122 theGang lvl123 lvl91) `cast` ...
    of nt2 { DVector ipv7 ipv8 ipv9 ipv10 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl121 theGang $fNumInt_$c+ lvl13 ipv7
               of _ { (di, n) ->
               case di `cast` ... of nt3 { DInt ipv11 ->
               case n of n1 { I# ipv12 ->
               case >=# ipv12 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv12)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv12 4) (s1 `cast` ...)
                   of _ { (# ipv13, ipv14 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt4 :: R:Dist(,) Int (Vector Int)
                     nt4 = DProd (nt3 `cast` ...) (nt2 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl120 (nt4 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv15, ipv16 #) ->
                               case ipv16 of _ { (x, y) ->
                               case x of _ { I# ipv17 ->
                               case y `cast` ... of _ { Vector ipv18 ipv19 ipv20 ->
                               case copyByteArray#
                                      ipv20
                                      (*# ipv18 4)
                                      ipv14
                                      (*# ipv17 4)
                                      (*# ipv19 4)
                                      (ipv15 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv13 `cast` ...))
                        `cast` ...
                   of _ { (# ipv15, _ #) ->
                   case unsafeFreezeByteArray# ipv14 (ipv15 `cast` ...)
                   of _ { (# ipv17, ipv18 #) ->
                   (# ipv17 `cast` ..., (Vector 0 ipv12 ipv18) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv11 ipv12 ipv13 ->
    case newByteArray# (*# (+# ipv5 ipv12) 4) (s `cast` ...)
    of _ { (# ipv14, ipv15 #) ->
    letrec {
      $s$wa
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv12 of _ {
            False ->
              case indexIntArray# ipv13 (+# ipv11 sc1) of wild { __DEFAULT ->
              case writeIntArray# ipv15 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> (# sc2, I# sc #)
          }; } in
    letrec {
      $s$wa1
        :: Int#
           -> State# (PrimState (ST s))
           -> Int#
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa1 =
        \ (sc :: Int#) (sc1 :: State# (PrimState (ST s))) (sc2 :: Int#) ->
          case >=# sc2 ipv5 of _ {
            False ->
              case indexIntArray# ipv6 (+# ipv4 sc2) of wild { __DEFAULT ->
              case writeIntArray# ipv15 sc wild (sc1 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa1 (+# sc 1) (s'# `cast` ...) (+# sc2 1)
              }
              };
            True -> $s$wa sc 0 sc1
          }; } in
    case $s$wa1 0 (ipv14 `cast` ...) 0 of _ { (# ipv16, ipv17 #) ->
    case ipv17 of _ { I# tpl1 ->
    case unsafeFreezeByteArray# (ipv15 `cast` ...) ipv16
    of _ { (# ipv18, ipv19 #) ->
    case newByteArray# 4 ipv18 of _ { (# ipv20, ipv21 #) ->
    case writeIntArray# ipv21 0 0 ipv20 of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv21 s'# of _ { (# ipv22, ipv23 #) ->
    case newByteArray# 4 ipv22 of _ { (# ipv24, ipv25 #) ->
    case writeIntArray# ipv25 0 tpl1 ipv24 of s'#1 { __DEFAULT ->
    case unsafeFreezeByteArray# ipv25 s'#1 of _ { (# ipv26, ipv27 #) ->
    case newArrayArray# 1 (ipv26 `cast` ...)
    of _ { (# ipv28, ipv29 #) ->
    case writeByteArrayArray# ipv29 0 ipv19 ipv28
    of s'#2 { __DEFAULT ->
    case unsafeFreezeArrayArray# ipv29 s'#2
    of _ { (# ipv30, ipv31 #) ->
    (# ipv30, Vectors 1 ipv23 ipv27 ipv31 #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vexample33 :: Arrays Int
$vexample33 = runSTRep $vexample34

$vexample32 :: R:PDatasInt
$vexample32 = PInts $vexample33

$vexample_vsegids1 :: Vector Int
$vexample_vsegids1 = runSTRep $vexample13

$vexample_tpl2 :: Vector Int
$vexample_tpl2 = runSTRep $vexample14

$vexample_tpl1 :: Vector Int
$vexample_tpl1 = runSTRep $vexample14

$vexample30 :: forall s. State# s -> (# State# s, Vector Int #)
$vexample30 =
  \ (@ s) (s :: State# s) ->
    case newByteArray# 4 (s `cast` ...) of _ { (# ipv, ipv1 #) ->
    case writeIntArray# ipv1 0 2 ipv of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv1 s'# of _ { (# ipv2, ipv3 #) ->
    (# ipv2 `cast` ..., (Vector 0 1 ipv3) `cast` ... #)
    }
    }
    }

$vexample_ussegd1 :: UPSSegd
$vexample_ussegd1 =
  case (runSTRep $vexample30) `cast` ...
  of nt { Vector ipv1 ipv2 ipv3 ->
  case (runSTRep $vexample14) `cast` ...
  of nt1 { Vector ipv4 ipv5 ipv6 ->
  UPSSegd
    True
    $vexample_tpl2
    $vexample_tpl1
    (nt `cast` ...)
    (nt1 `cast` ...)
    2
    (splitSSegdOnElemsD
       theGang
       (USSegd
          True
          $vexample_tpl2
          $vexample_tpl1
          (nt `cast` ...)
          (nt1 `cast` ...)
          2))
  }
  }

$vexample29 :: Dist ((USegd, Int), Int)
$vexample29 =
  case $vexample_ussegd1 of _ { UPSSegd ww ww1 ww2 ww3 ww4 ww5 ww6 ->
  $wmkDist $vexample_vsegids1 ww ww3 ww4
  }

$vexample_vsegd1 :: VSegd
$vexample_vsegd1 =
  UPVSegd
    False
    $vexample_vsegids1
    $vexample_vsegids1
    $vexample_ussegd1
    $vexample_ussegd1
    $vexample29

$vexample60 :: forall s. State# s -> (# State# s, Vectors Double #)
$vexample60 =
  \ (@ s) (s :: State# s) ->
    case newByteArray# 0 (s `cast` ...) of _ { (# ipv, ipv1 #) ->
    case unsafeFreezeByteArray# (ipv1 `cast` ...) (ipv `cast` ...)
    of _ { (# ipv2, ipv3 #) ->
    case newByteArray# 4 ipv2 of _ { (# ipv4, ipv5 #) ->
    case writeIntArray# ipv5 0 0 ipv4 of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv5 s'# of _ { (# ipv6, ipv7 #) ->
    case newByteArray# 4 ipv6 of _ { (# ipv8, ipv9 #) ->
    case writeIntArray# ipv9 0 0 ipv8 of s'#1 { __DEFAULT ->
    case unsafeFreezeByteArray# ipv9 s'#1 of _ { (# ipv10, ipv11 #) ->
    case newArrayArray# 1 (ipv10 `cast` ...)
    of _ { (# ipv12, ipv13 #) ->
    case writeByteArrayArray# ipv13 0 ipv3 ipv12 of s'#2 { __DEFAULT ->
    case unsafeFreezeArrayArray# ipv13 s'#2
    of _ { (# ipv14, ipv15 #) ->
    (# ipv14, Vectors 1 ipv7 ipv11 ipv15 #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vexample59 :: Arrays Double
$vexample59 = runSTRep $vexample60

$vexample58 :: R:PDatasDouble
$vexample58 = PDoubles $vexample59

$vexample57 :: forall s. State# s -> (# State# s, Vectors Int #)
$vexample57 =
  \ (@ s) (s :: State# s) ->
    case newByteArray# 0 (s `cast` ...) of _ { (# ipv, ipv1 #) ->
    case unsafeFreezeByteArray# (ipv1 `cast` ...) (ipv `cast` ...)
    of _ { (# ipv2, ipv3 #) ->
    case newByteArray# 4 ipv2 of _ { (# ipv4, ipv5 #) ->
    case writeIntArray# ipv5 0 0 ipv4 of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv5 s'# of _ { (# ipv6, ipv7 #) ->
    case newByteArray# 4 ipv6 of _ { (# ipv8, ipv9 #) ->
    case writeIntArray# ipv9 0 0 ipv8 of s'#1 { __DEFAULT ->
    case unsafeFreezeByteArray# ipv9 s'#1 of _ { (# ipv10, ipv11 #) ->
    case newArrayArray# 1 (ipv10 `cast` ...)
    of _ { (# ipv12, ipv13 #) ->
    case writeByteArrayArray# ipv13 0 ipv3 ipv12 of s'#2 { __DEFAULT ->
    case unsafeFreezeArrayArray# ipv13 s'#2
    of _ { (# ipv14, ipv15 #) ->
    (# ipv14, Vectors 1 ipv7 ipv11 ipv15 #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vexample56 :: Arrays Int
$vexample56 = runSTRep $vexample57

$vexample55 :: R:PDatasInt
$vexample55 = PInts $vexample56

$vexample_vsegids4 :: Vector Int
$vexample_vsegids4 = runSTRep $vexample13

$vexample_tpl8 :: Vector Int
$vexample_tpl8 = runSTRep $vexample14

$vexample_tpl5 :: Vector Int
$vexample_tpl5 = runSTRep $vexample14

$vexample_ussegd3 :: UPSSegd
$vexample_ussegd3 =
  case (runSTRep $vexample14) `cast` ...
  of nt { Vector ipv1 ipv2 ipv3 ->
  case (runSTRep $vexample14) `cast` ...
  of nt1 { Vector ipv4 ipv5 ipv6 ->
  UPSSegd
    True
    $vexample_tpl8
    $vexample_tpl5
    (nt `cast` ...)
    (nt1 `cast` ...)
    0
    (splitSSegdOnElemsD
       theGang
       (USSegd
          True
          $vexample_tpl8
          $vexample_tpl5
          (nt `cast` ...)
          (nt1 `cast` ...)
          0))
  }
  }

$vexample53 :: Dist ((USegd, Int), Int)
$vexample53 =
  case $vexample_ussegd3 of _ { UPSSegd ww ww1 ww2 ww3 ww4 ww5 ww6 ->
  $wmkDist $vexample_vsegids4 ww ww3 ww4
  }

$vexample_vsegd3 :: VSegd
$vexample_vsegd3 =
  UPVSegd
    False
    $vexample_vsegids4
    $vexample_vsegids4
    $vexample_ussegd3
    $vexample_ussegd3
    $vexample53

lvl128 :: DT (Int, Vector Double)
lvl128 = $fDT(,) $fDTInt $dDT2

lvl129 :: What
lvl129 = What lvl11

lvl130 :: What
lvl130 = What lvl88

lvl131 :: Int -> Int -> Vector Double
lvl131 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 2.1 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl132 :: DT (Int, Vector Double)
lvl132 = $fDT(,) $fDTInt $dDT2

lvl133 :: What
lvl133 = What lvl11

lvl134 :: What
lvl134 = What lvl88

lvl135 :: Int -> Int -> Vector Double
lvl135 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 1.1 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl136 :: DT (Int, Vector Double)
lvl136 = $fDT(,) $fDTInt $dDT2

lvl137 :: What
lvl137 = What lvl11

lvl138 :: What
lvl138 = What lvl88

lvl139 :: Int -> Int -> Vector Double
lvl139 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 31.2 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl140 :: DT (Int, Vector Double)
lvl140 = $fDT(,) $fDTInt $dDT2

lvl141 :: What
lvl141 = What lvl11

lvl142 :: What
lvl142 = What lvl88

lvl143 :: Int -> Int -> Vector Double
lvl143 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 21.1 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

$vexample49 :: forall s. State# s -> (# State# s, Vectors Double #)
$vexample49 =
  \ (@ s) (s :: State# s) ->
    case (imapD' $fDTInt $dDT2 lvl142 theGang lvl143 lvl91) `cast` ...
    of nt { DVector ipv ipv1 ipv2 ipv3 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl141 theGang $fNumInt_$c+ lvl13 ipv
               of _ { (di, n) ->
               case di `cast` ... of nt1 { DInt ipv4 ->
               case n of n1 { I# ipv5 ->
               case >=# ipv5 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv5 8) (s1 `cast` ...)
                   of _ { (# ipv6, ipv7 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt2 :: R:Dist(,) Int (Vector Double)
                     nt2 = DProd (nt1 `cast` ...) (nt `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl140 (nt2 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv10, ipv11 #) ->
                               case ipv11 of _ { (x, y) ->
                               case x of _ { I# ipv12 ->
                               case y `cast` ... of _ { Vector ipv8 ipv9 ipv13 ->
                               case copyByteArray#
                                      ipv13
                                      (*# ipv8 8)
                                      ipv7
                                      (*# ipv12 8)
                                      (*# ipv9 8)
                                      (ipv10 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv6 `cast` ...))
                        `cast` ...
                   of _ { (# ipv10, _ #) ->
                   case unsafeFreezeByteArray# ipv7 (ipv10 `cast` ...)
                   of _ { (# ipv8, ipv9 #) ->
                   (# ipv8 `cast` ..., (Vector 0 ipv5 ipv9) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv4 ipv5 ipv6 ->
    case (imapD' $fDTInt $dDT2 lvl138 theGang lvl139 lvl91) `cast` ...
    of nt2 { DVector ipv7 ipv8 ipv9 ipv10 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl137 theGang $fNumInt_$c+ lvl13 ipv7
               of _ { (di, n) ->
               case di `cast` ... of nt3 { DInt ipv11 ->
               case n of n1 { I# ipv12 ->
               case >=# ipv12 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv12)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv12 8) (s1 `cast` ...)
                   of _ { (# ipv13, ipv14 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt4 :: R:Dist(,) Int (Vector Double)
                     nt4 = DProd (nt3 `cast` ...) (nt2 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl136 (nt4 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv15, ipv16 #) ->
                               case ipv16 of _ { (x, y) ->
                               case x of _ { I# ipv17 ->
                               case y `cast` ... of _ { Vector ipv18 ipv19 ipv20 ->
                               case copyByteArray#
                                      ipv20
                                      (*# ipv18 8)
                                      ipv14
                                      (*# ipv17 8)
                                      (*# ipv19 8)
                                      (ipv15 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv13 `cast` ...))
                        `cast` ...
                   of _ { (# ipv15, _ #) ->
                   case unsafeFreezeByteArray# ipv14 (ipv15 `cast` ...)
                   of _ { (# ipv17, ipv18 #) ->
                   (# ipv17 `cast` ..., (Vector 0 ipv12 ipv18) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv11 ipv12 ipv13 ->
    case (imapD' $fDTInt $dDT2 lvl134 theGang lvl135 lvl91) `cast` ...
    of nt4 { DVector ipv14 ipv15 ipv16 ipv17 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl133 theGang $fNumInt_$c+ lvl13 ipv14
               of _ { (di, n) ->
               case di `cast` ... of nt5 { DInt ipv18 ->
               case n of n1 { I# ipv19 ->
               case >=# ipv19 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv19)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv19 8) (s1 `cast` ...)
                   of _ { (# ipv20, ipv21 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt6 :: R:Dist(,) Int (Vector Double)
                     nt6 = DProd (nt5 `cast` ...) (nt4 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl132 (nt6 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv22, ipv23 #) ->
                               case ipv23 of _ { (x, y) ->
                               case x of _ { I# ipv24 ->
                               case y `cast` ... of _ { Vector ipv25 ipv26 ipv27 ->
                               case copyByteArray#
                                      ipv27
                                      (*# ipv25 8)
                                      ipv21
                                      (*# ipv24 8)
                                      (*# ipv26 8)
                                      (ipv22 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv20 `cast` ...))
                        `cast` ...
                   of _ { (# ipv22, _ #) ->
                   case unsafeFreezeByteArray# ipv21 (ipv22 `cast` ...)
                   of _ { (# ipv24, ipv25 #) ->
                   (# ipv24 `cast` ..., (Vector 0 ipv19 ipv25) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv18 ipv19 ipv20 ->
    case (imapD' $fDTInt $dDT2 lvl130 theGang lvl131 lvl91) `cast` ...
    of nt6 { DVector ipv21 ipv22 ipv23 ipv24 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl129 theGang $fNumInt_$c+ lvl13 ipv21
               of _ { (di, n) ->
               case di `cast` ... of nt7 { DInt ipv25 ->
               case n of n1 { I# ipv26 ->
               case >=# ipv26 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv26)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv26 8) (s1 `cast` ...)
                   of _ { (# ipv27, ipv28 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt8 :: R:Dist(,) Int (Vector Double)
                     nt8 = DProd (nt7 `cast` ...) (nt6 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl128 (nt8 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv29, ipv30 #) ->
                               case ipv30 of _ { (x, y) ->
                               case x of _ { I# ipv31 ->
                               case y `cast` ... of _ { Vector ipv32 ipv33 ipv34 ->
                               case copyByteArray#
                                      ipv34
                                      (*# ipv32 8)
                                      ipv28
                                      (*# ipv31 8)
                                      (*# ipv33 8)
                                      (ipv29 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv27 `cast` ...))
                        `cast` ...
                   of _ { (# ipv29, _ #) ->
                   case unsafeFreezeByteArray# ipv28 (ipv29 `cast` ...)
                   of _ { (# ipv31, ipv32 #) ->
                   (# ipv31 `cast` ..., (Vector 0 ipv26 ipv32) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv25 ipv26 ipv27 ->
    case newByteArray#
           (*# (+# ipv5 (+# ipv12 (+# ipv19 ipv26))) 8) (s `cast` ...)
    of _ { (# ipv28, ipv29 #) ->
    letrec {
      $s$wa
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv26 of _ {
            False ->
              case indexDoubleArray# ipv27 (+# ipv25 sc1) of wild { __DEFAULT ->
              case writeDoubleArray# ipv29 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> (# sc2, I# sc #)
          }; } in
    letrec {
      $s$wa1
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa1 =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv19 of _ {
            False ->
              case indexDoubleArray# ipv20 (+# ipv18 sc1) of wild { __DEFAULT ->
              case writeDoubleArray# ipv29 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa1 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> $s$wa sc 0 sc2
          }; } in
    letrec {
      $s$wa2
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa2 =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv12 of _ {
            False ->
              case indexDoubleArray# ipv13 (+# ipv11 sc1) of wild { __DEFAULT ->
              case writeDoubleArray# ipv29 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa2 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> $s$wa1 sc 0 sc2
          }; } in
    letrec {
      $s$wa3
        :: Int#
           -> State# (PrimState (ST s))
           -> Int#
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa3 =
        \ (sc :: Int#) (sc1 :: State# (PrimState (ST s))) (sc2 :: Int#) ->
          case >=# sc2 ipv5 of _ {
            False ->
              case indexDoubleArray# ipv6 (+# ipv4 sc2) of wild { __DEFAULT ->
              case writeDoubleArray# ipv29 sc wild (sc1 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa3 (+# sc 1) (s'# `cast` ...) (+# sc2 1)
              }
              };
            True -> $s$wa2 sc 0 sc1
          }; } in
    case $s$wa3 0 (ipv28 `cast` ...) 0 of _ { (# ipv30, ipv31 #) ->
    case ipv31 of _ { I# tpl1 ->
    case unsafeFreezeByteArray# (ipv29 `cast` ...) ipv30
    of _ { (# ipv32, ipv33 #) ->
    case newByteArray# 4 ipv32 of _ { (# ipv34, ipv35 #) ->
    case writeIntArray# ipv35 0 0 ipv34 of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv35 s'# of _ { (# ipv36, ipv37 #) ->
    case newByteArray# 4 ipv36 of _ { (# ipv38, ipv39 #) ->
    case writeIntArray# ipv39 0 tpl1 ipv38 of s'#1 { __DEFAULT ->
    case unsafeFreezeByteArray# ipv39 s'#1 of _ { (# ipv40, ipv41 #) ->
    case newArrayArray# 1 (ipv40 `cast` ...)
    of _ { (# ipv42, ipv43 #) ->
    case writeByteArrayArray# ipv43 0 ipv33 ipv42
    of s'#2 { __DEFAULT ->
    case unsafeFreezeArrayArray# ipv43 s'#2
    of _ { (# ipv44, ipv45 #) ->
    (# ipv44, Vectors 1 ipv37 ipv41 ipv45 #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vexample48 :: Arrays Double
$vexample48 = runSTRep $vexample49

$vexample47 :: R:PDatasDouble
$vexample47 = PDoubles $vexample48

lvl144 :: DT (Int, Vector Int)
lvl144 = $fDT(,) $fDTInt $dDT1

lvl145 :: What
lvl145 = What lvl11

lvl146 :: What
lvl146 = What lvl88

lvl147 :: Int -> Int -> Vector Int
lvl147 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Int #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 4) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeIntArray# ipv2 sc 4 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl148 :: DT (Int, Vector Int)
lvl148 = $fDT(,) $fDTInt $dDT1

lvl149 :: What
lvl149 = What lvl11

lvl150 :: What
lvl150 = What lvl88

lvl151 :: Int -> Int -> Vector Int
lvl151 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Int #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 4) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeIntArray# ipv2 sc 3 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl152 :: DT (Int, Vector Int)
lvl152 = $fDT(,) $fDTInt $dDT1

lvl153 :: What
lvl153 = What lvl11

lvl154 :: What
lvl154 = What lvl88

lvl155 :: Int -> Int -> Vector Int
lvl155 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Int #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 4) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeIntArray# ipv2 sc 1 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl156 :: DT (Int, Vector Int)
lvl156 = $fDT(,) $fDTInt $dDT1

lvl157 :: What
lvl157 = What lvl11

lvl158 :: What
lvl158 = What lvl88

lvl159 :: Int -> Int -> Vector Int
lvl159 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Int #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 4) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeIntArray# ipv2 sc 0 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

$vexample46 :: forall s. State# s -> (# State# s, Vectors Int #)
$vexample46 =
  \ (@ s) (s :: State# s) ->
    case (imapD' $fDTInt $dDT1 lvl158 theGang lvl159 lvl91) `cast` ...
    of nt { DVector ipv ipv1 ipv2 ipv3 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl157 theGang $fNumInt_$c+ lvl13 ipv
               of _ { (di, n) ->
               case di `cast` ... of nt1 { DInt ipv4 ->
               case n of n1 { I# ipv5 ->
               case >=# ipv5 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv5 4) (s1 `cast` ...)
                   of _ { (# ipv6, ipv7 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt2 :: R:Dist(,) Int (Vector Int)
                     nt2 = DProd (nt1 `cast` ...) (nt `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl156 (nt2 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv10, ipv11 #) ->
                               case ipv11 of _ { (x, y) ->
                               case x of _ { I# ipv12 ->
                               case y `cast` ... of _ { Vector ipv8 ipv9 ipv13 ->
                               case copyByteArray#
                                      ipv13
                                      (*# ipv8 4)
                                      ipv7
                                      (*# ipv12 4)
                                      (*# ipv9 4)
                                      (ipv10 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv6 `cast` ...))
                        `cast` ...
                   of _ { (# ipv10, _ #) ->
                   case unsafeFreezeByteArray# ipv7 (ipv10 `cast` ...)
                   of _ { (# ipv8, ipv9 #) ->
                   (# ipv8 `cast` ..., (Vector 0 ipv5 ipv9) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv4 ipv5 ipv6 ->
    case (imapD' $fDTInt $dDT1 lvl154 theGang lvl155 lvl91) `cast` ...
    of nt2 { DVector ipv7 ipv8 ipv9 ipv10 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl153 theGang $fNumInt_$c+ lvl13 ipv7
               of _ { (di, n) ->
               case di `cast` ... of nt3 { DInt ipv11 ->
               case n of n1 { I# ipv12 ->
               case >=# ipv12 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv12)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv12 4) (s1 `cast` ...)
                   of _ { (# ipv13, ipv14 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt4 :: R:Dist(,) Int (Vector Int)
                     nt4 = DProd (nt3 `cast` ...) (nt2 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl152 (nt4 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv15, ipv16 #) ->
                               case ipv16 of _ { (x, y) ->
                               case x of _ { I# ipv17 ->
                               case y `cast` ... of _ { Vector ipv18 ipv19 ipv20 ->
                               case copyByteArray#
                                      ipv20
                                      (*# ipv18 4)
                                      ipv14
                                      (*# ipv17 4)
                                      (*# ipv19 4)
                                      (ipv15 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv13 `cast` ...))
                        `cast` ...
                   of _ { (# ipv15, _ #) ->
                   case unsafeFreezeByteArray# ipv14 (ipv15 `cast` ...)
                   of _ { (# ipv17, ipv18 #) ->
                   (# ipv17 `cast` ..., (Vector 0 ipv12 ipv18) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv11 ipv12 ipv13 ->
    case (imapD' $fDTInt $dDT1 lvl150 theGang lvl151 lvl91) `cast` ...
    of nt4 { DVector ipv14 ipv15 ipv16 ipv17 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl149 theGang $fNumInt_$c+ lvl13 ipv14
               of _ { (di, n) ->
               case di `cast` ... of nt5 { DInt ipv18 ->
               case n of n1 { I# ipv19 ->
               case >=# ipv19 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv19)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv19 4) (s1 `cast` ...)
                   of _ { (# ipv20, ipv21 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt6 :: R:Dist(,) Int (Vector Int)
                     nt6 = DProd (nt5 `cast` ...) (nt4 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl148 (nt6 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv22, ipv23 #) ->
                               case ipv23 of _ { (x, y) ->
                               case x of _ { I# ipv24 ->
                               case y `cast` ... of _ { Vector ipv25 ipv26 ipv27 ->
                               case copyByteArray#
                                      ipv27
                                      (*# ipv25 4)
                                      ipv21
                                      (*# ipv24 4)
                                      (*# ipv26 4)
                                      (ipv22 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv20 `cast` ...))
                        `cast` ...
                   of _ { (# ipv22, _ #) ->
                   case unsafeFreezeByteArray# ipv21 (ipv22 `cast` ...)
                   of _ { (# ipv24, ipv25 #) ->
                   (# ipv24 `cast` ..., (Vector 0 ipv19 ipv25) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv18 ipv19 ipv20 ->
    case (imapD' $fDTInt $dDT1 lvl146 theGang lvl147 lvl91) `cast` ...
    of nt6 { DVector ipv21 ipv22 ipv23 ipv24 ->
    case (runSTRep
            (\ (@ s1) (s1 :: State# s1) ->
               case scanD $fDTInt lvl145 theGang $fNumInt_$c+ lvl13 ipv21
               of _ { (di, n) ->
               case di `cast` ... of nt7 { DInt ipv25 ->
               case n of n1 { I# ipv26 ->
               case >=# ipv26 0 of _ {
                 False ->
                   case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv26)
                   of wild1 {
                   };
                 True ->
                   case newByteArray# (*# ipv26 4) (s1 `cast` ...)
                   of _ { (# ipv27, ipv28 #) ->
                   case theGang of _ { Gang ww ww1 ww2 ->
                   let {
                     nt8 :: R:Dist(,) Int (Vector Int)
                     nt8 = DProd (nt7 `cast` ...) (nt6 `cast` ...) } in
                   case ($wa1
                           ww
                           ww1
                           ww2
                           (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                           (WorkCopy n1)
                           ((\ (i :: Int) (s2 :: State# s1) ->
                               case ((((myD lvl144 (nt8 `cast` ...)) `cast` ...) i) `cast` ...) s2
                               of _ { (# ipv29, ipv30 #) ->
                               case ipv30 of _ { (x, y) ->
                               case x of _ { I# ipv31 ->
                               case y `cast` ... of _ { Vector ipv32 ipv33 ipv34 ->
                               case copyByteArray#
                                      ipv34
                                      (*# ipv32 4)
                                      ipv28
                                      (*# ipv31 4)
                                      (*# ipv33 4)
                                      (ipv29 `cast` ...)
                               of s'# { __DEFAULT ->
                               (# s'#, () #) `cast` ...
                               }
                               }
                               }
                               }
                               })
                            `cast` ...)
                           (ipv27 `cast` ...))
                        `cast` ...
                   of _ { (# ipv29, _ #) ->
                   case unsafeFreezeByteArray# ipv28 (ipv29 `cast` ...)
                   of _ { (# ipv31, ipv32 #) ->
                   (# ipv31 `cast` ..., (Vector 0 ipv26 ipv32) `cast` ... #)
                   }
                   }
                   }
                   }
               }
               }
               }
               }))
         `cast` ...
    of _ { Vector ipv25 ipv26 ipv27 ->
    case newByteArray#
           (*# (+# ipv5 (+# ipv12 (+# ipv19 ipv26))) 4) (s `cast` ...)
    of _ { (# ipv28, ipv29 #) ->
    letrec {
      $s$wa
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv26 of _ {
            False ->
              case indexIntArray# ipv27 (+# ipv25 sc1) of wild { __DEFAULT ->
              case writeIntArray# ipv29 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> (# sc2, I# sc #)
          }; } in
    letrec {
      $s$wa1
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa1 =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv19 of _ {
            False ->
              case indexIntArray# ipv20 (+# ipv18 sc1) of wild { __DEFAULT ->
              case writeIntArray# ipv29 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa1 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> $s$wa sc 0 sc2
          }; } in
    letrec {
      $s$wa2
        :: Int#
           -> Int#
           -> State# (PrimState (ST s))
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa2 =
        \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# (PrimState (ST s))) ->
          case >=# sc1 ipv12 of _ {
            False ->
              case indexIntArray# ipv13 (+# ipv11 sc1) of wild { __DEFAULT ->
              case writeIntArray# ipv29 sc wild (sc2 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa2 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
              }
              };
            True -> $s$wa1 sc 0 sc2
          }; } in
    letrec {
      $s$wa3
        :: Int#
           -> State# (PrimState (ST s))
           -> Int#
           -> (# State# (PrimState (ST s)), Int #)
      $s$wa3 =
        \ (sc :: Int#) (sc1 :: State# (PrimState (ST s))) (sc2 :: Int#) ->
          case >=# sc2 ipv5 of _ {
            False ->
              case indexIntArray# ipv6 (+# ipv4 sc2) of wild { __DEFAULT ->
              case writeIntArray# ipv29 sc wild (sc1 `cast` ...)
              of s'# { __DEFAULT ->
              $s$wa3 (+# sc 1) (s'# `cast` ...) (+# sc2 1)
              }
              };
            True -> $s$wa2 sc 0 sc1
          }; } in
    case $s$wa3 0 (ipv28 `cast` ...) 0 of _ { (# ipv30, ipv31 #) ->
    case ipv31 of _ { I# tpl1 ->
    case unsafeFreezeByteArray# (ipv29 `cast` ...) ipv30
    of _ { (# ipv32, ipv33 #) ->
    case newByteArray# 4 ipv32 of _ { (# ipv34, ipv35 #) ->
    case writeIntArray# ipv35 0 0 ipv34 of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv35 s'# of _ { (# ipv36, ipv37 #) ->
    case newByteArray# 4 ipv36 of _ { (# ipv38, ipv39 #) ->
    case writeIntArray# ipv39 0 tpl1 ipv38 of s'#1 { __DEFAULT ->
    case unsafeFreezeByteArray# ipv39 s'#1 of _ { (# ipv40, ipv41 #) ->
    case newArrayArray# 1 (ipv40 `cast` ...)
    of _ { (# ipv42, ipv43 #) ->
    case writeByteArrayArray# ipv43 0 ipv33 ipv42
    of s'#2 { __DEFAULT ->
    case unsafeFreezeArrayArray# ipv43 s'#2
    of _ { (# ipv44, ipv45 #) ->
    (# ipv44, Vectors 1 ipv37 ipv41 ipv45 #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

$vexample45 :: Arrays Int
$vexample45 = runSTRep $vexample46

$vexample44 :: R:PDatasInt
$vexample44 = PInts $vexample45

$vexample_vsegids3 :: Vector Int
$vexample_vsegids3 = runSTRep $vexample13

$vexample_tpl4 :: Vector Int
$vexample_tpl4 = runSTRep $vexample14

$vexample_tpl3 :: Vector Int
$vexample_tpl3 = runSTRep $vexample14

$vexample42 :: forall s. State# s -> (# State# s, Vector Int #)
$vexample42 =
  \ (@ s) (s :: State# s) ->
    case newByteArray# 4 (s `cast` ...) of _ { (# ipv, ipv1 #) ->
    case writeIntArray# ipv1 0 4 ipv of s'# { __DEFAULT ->
    case unsafeFreezeByteArray# ipv1 s'# of _ { (# ipv2, ipv3 #) ->
    (# ipv2 `cast` ..., (Vector 0 1 ipv3) `cast` ... #)
    }
    }
    }

$vexample_ussegd2 :: UPSSegd
$vexample_ussegd2 =
  case (runSTRep $vexample42) `cast` ...
  of nt { Vector ipv1 ipv2 ipv3 ->
  case (runSTRep $vexample14) `cast` ...
  of nt1 { Vector ipv4 ipv5 ipv6 ->
  UPSSegd
    True
    $vexample_tpl4
    $vexample_tpl3
    (nt `cast` ...)
    (nt1 `cast` ...)
    4
    (splitSSegdOnElemsD
       theGang
       (USSegd
          True
          $vexample_tpl4
          $vexample_tpl3
          (nt `cast` ...)
          (nt1 `cast` ...)
          4))
  }
  }

$vexample41 :: Dist ((USegd, Int), Int)
$vexample41 =
  case $vexample_ussegd2 of _ { UPSSegd ww ww1 ww2 ww3 ww4 ww5 ww6 ->
  $wmkDist $vexample_vsegids3 ww ww3 ww4
  }

$vexample_vsegd2 :: VSegd
$vexample_vsegd2 =
  UPVSegd
    False
    $vexample_vsegids3
    $vexample_vsegids3
    $vexample_ussegd2
    $vexample_ussegd2
    $vexample41

$vexample16 :: R:PDatas(,) (Wrap Double) (Wrap Int)
$vexample16 =
  PTuple2s ($vexample20 `cast` ...) ($vexample17 `cast` ...)

$vexample26 :: UPSegd
$vexample26 = unsafeDemoteToUPSegd $vexample_vsegd

$vexample11 :: PData (PRepr (Double, Int))
$vexample11 =
  extractvs_delay
    ($vexample23 `cast` ...) ($vexample16 `cast` ...) $vexample_vsegd

$vexample10 :: R:PDataPArray (PRepr (Double, Int))
$vexample10 =
  PNested
    $vexample_vsegd ($vexample16 `cast` ...) $vexample26 $vexample11

$vexample31 :: R:PDatas(,) (Wrap Double) (Wrap Int)
$vexample31 =
  PTuple2s ($vexample35 `cast` ...) ($vexample32 `cast` ...)

$vexample38 :: UPSegd
$vexample38 = unsafeDemoteToUPSegd $vexample_vsegd1

$vexample28 :: PData (PRepr (Double, Int))
$vexample28 =
  extractvs_delay
    ($vexample23 `cast` ...) ($vexample31 `cast` ...) $vexample_vsegd1

$vexample27 :: R:PDataPArray (PRepr (Double, Int))
$vexample27 =
  PNested
    $vexample_vsegd1 ($vexample31 `cast` ...) $vexample38 $vexample28

$vexample54 :: R:PDatas(,) (Wrap Double) (Wrap Int)
$vexample54 =
  PTuple2s ($vexample58 `cast` ...) ($vexample55 `cast` ...)

$vexample61 :: UPSegd
$vexample61 = unsafeDemoteToUPSegd $vexample_vsegd3

$vexample52 :: PData (PRepr (Double, Int))
$vexample52 =
  extractvs_delay
    ($vexample23 `cast` ...) ($vexample54 `cast` ...) $vexample_vsegd3

$vexample51 :: R:PDataPArray (PRepr (Double, Int))
$vexample51 =
  PNested
    $vexample_vsegd3 ($vexample54 `cast` ...) $vexample61 $vexample52

$vexample43 :: R:PDatas(,) (Wrap Double) (Wrap Int)
$vexample43 =
  PTuple2s ($vexample47 `cast` ...) ($vexample44 `cast` ...)

$vexample50 :: UPSegd
$vexample50 = unsafeDemoteToUPSegd $vexample_vsegd2

$vexample40 :: PData (PRepr (Double, Int))
$vexample40 =
  extractvs_delay
    ($vexample23 `cast` ...) ($vexample43 `cast` ...) $vexample_vsegd2

$vexample39 :: R:PDataPArray (PRepr (Double, Int))
$vexample39 =
  PNested
    $vexample_vsegd2 ($vexample43 `cast` ...) $vexample50 $vexample40

$vexample9 :: PData (V:GHC:PArr_[::] (Double, Int))
$vexample9 =
  case ($fPRPArray_$cappendPR
          ($vexample23 `cast` ...)
          ($vexample51 `cast` ...)
          ($vexample39 `cast` ...))
       `cast` ...
  of _ { PNested vsegd xs segd flat ->
  case ($fPRPArray_$cappendPR
          ($vexample23 `cast` ...)
          ($vexample27 `cast` ...)
          ((PNested vsegd xs segd flat) `cast` ...))
       `cast` ...
  of _ { PNested vsegd1 xs1 segd1 flat1 ->
  case ($fPRPArray_$cappendPR
          ($vexample23 `cast` ...)
          ($vexample10 `cast` ...)
          ((PNested vsegd1 xs1 segd1 flat1) `cast` ...))
       `cast` ...
  of _ { PNested vsegd2 xs2 segd2 flat2 ->
  (PNested
     vsegd2
     (case xs2 `cast` ... of _ { PTuple2s ds1 ds2 ->
      (PTuple2s (ds1 `cast` ...) (ds2 `cast` ...)) `cast` ...
      })
     segd2
     (case flat2 `cast` ... of _ { PTuple2 ds1 ds2 ->
      (PTuple2 (ds1 `cast` ...) (ds2 `cast` ...)) `cast` ...
      }))
  `cast` ...
  }
  }
  }

$vexample4 :: What
$vexample4 = What $vexample5

$vexample2 :: What
$vexample2 = What $vexample3

$vexample1 :: What
$vexample1 = WFMapGen $vexample2 WSlice

lvl160 :: DT (Int, Vector Double)
lvl160 = $fDT(,) $fDTInt $dDT2

lvl161 :: What
lvl161 = What lvl11

lvl162 :: What
lvl162 = What lvl88

lvl163 :: Int -> Int -> Vector Double
lvl163 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 0.3 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl164 :: Dist Int
lvl164 = splitLenD theGang len

lvl165 :: DT (Int, Vector Double)
lvl165 = $fDT(,) $fDTInt $dDT2

lvl166 :: What
lvl166 = What lvl11

lvl167 :: What
lvl167 = What lvl88

lvl168 :: Int -> Int -> Vector Double
lvl168 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 6.5 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl169 :: Dist Int
lvl169 = splitLenD theGang len

lvl170 :: DT (Int, Vector Double)
lvl170 = $fDT(,) $fDTInt $dDT2

lvl171 :: What
lvl171 = What lvl11

lvl172 :: What
lvl172 = What lvl88

lvl173 :: Int -> Int -> Vector Double
lvl173 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 2.3 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl174 :: Dist Int
lvl174 = splitLenD theGang len

lvl175 :: DT (Int, Vector Double)
lvl175 = $fDT(,) $fDTInt $dDT2

lvl176 :: What
lvl176 = What lvl11

lvl177 :: What
lvl177 = What lvl88

lvl178 :: Int -> Int -> Vector Double
lvl178 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 1.2 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl179 :: Dist Int
lvl179 = splitLenD theGang len

lvl180 :: DT (Int, Vector Double)
lvl180 = $fDT(,) $fDTInt $dDT2

lvl181 :: What
lvl181 = What lvl11

lvl182 :: What
lvl182 = What lvl88

lvl183 :: Int -> Int -> Vector Double
lvl183 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 4.2 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl184 :: Dist Int
lvl184 = splitLenD theGang len

lvl185 :: DT (Int, Vector Double)
lvl185 = $fDT(,) $fDTInt $dDT2

lvl186 :: What
lvl186 = What lvl11

lvl187 :: What
lvl187 = What lvl88

lvl188 :: Int -> Int -> Vector Double
lvl188 =
  \ _ (x :: Int) ->
    case x of _ { I# ipv ->
    runSTRep
      (\ (@ s) (s :: State# s) ->
         let {
           $j :: Int# -> (# State# s, Vector Double #)
           $j =
             \ (x1 :: Int#) ->
               case newByteArray# (*# x1 8) (s `cast` ...)
               of _ { (# ipv1, ipv2 #) ->
               letrec {
                 $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
                 $s$wa =
                   \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
                     case <=# sc1 0 of _ {
                       False ->
                         case writeDoubleArray# ipv2 sc 2.4 (sc2 `cast` ...)
                         of s'# { __DEFAULT ->
                         $s$wa (+# sc 1) (-# sc1 1) (s'# `cast` ...)
                         };
                       True -> (# sc2, I# sc #)
                     }; } in
               case $s$wa 0 ipv (ipv1 `cast` ...) of _ { (# ipv5, ipv6 #) ->
               case ipv6 of _ { I# tpl1 ->
               case unsafeFreezeByteArray# ipv2 (ipv5 `cast` ...)
               of _ { (# ipv3, ipv4 #) ->
               (# ipv3 `cast` ..., (Vector 0 tpl1 ipv4) `cast` ... #)
               }
               }
               }
               } } in
         case <=# ipv 0 of _ {
           False -> $j ipv;
           True -> $j 0
         })
    }

lvl189 :: Dist Int
lvl189 = splitLenD theGang len

$vexample8 :: Array Double
$vexample8 =
  case (imapD' $fDTInt $dDT2 lvl187 theGang lvl188 lvl189) `cast` ...
  of nt { DVector ipv ipv1 ipv2 ipv3 ->
  case (runSTRep
          (\ (@ s) (s :: State# s) ->
             case scanD $fDTInt lvl186 theGang $fNumInt_$c+ lvl13 ipv
             of _ { (di, n) ->
             case di `cast` ... of nt1 { DInt ipv4 ->
             case n of n1 { I# ipv5 ->
             case >=# ipv5 0 of _ {
               False ->
                 case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv5)
                 of wild1 {
                 };
               True ->
                 case newByteArray# (*# ipv5 8) (s `cast` ...)
                 of _ { (# ipv6, ipv7 #) ->
                 case theGang of _ { Gang ww ww1 ww2 ->
                 let {
                   nt2 :: R:Dist(,) Int (Vector Double)
                   nt2 = DProd (nt1 `cast` ...) (nt `cast` ...) } in
                 case ($wa1
                         ww
                         ww1
                         ww2
                         (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                         (WorkCopy n1)
                         ((\ (i :: Int) (s1 :: State# s) ->
                             case ((((myD lvl185 (nt2 `cast` ...)) `cast` ...) i) `cast` ...) s1
                             of _ { (# ipv10, ipv11 #) ->
                             case ipv11 of _ { (x, y) ->
                             case x of _ { I# ipv12 ->
                             case y `cast` ... of _ { Vector ipv8 ipv9 ipv13 ->
                             case copyByteArray#
                                    ipv13
                                    (*# ipv8 8)
                                    ipv7
                                    (*# ipv12 8)
                                    (*# ipv9 8)
                                    (ipv10 `cast` ...)
                             of s'# { __DEFAULT ->
                             (# s'#, () #) `cast` ...
                             }
                             }
                             }
                             }
                             })
                          `cast` ...)
                         (ipv6 `cast` ...))
                      `cast` ...
                 of _ { (# ipv10, _ #) ->
                 case unsafeFreezeByteArray# ipv7 (ipv10 `cast` ...)
                 of _ { (# ipv8, ipv9 #) ->
                 (# ipv8 `cast` ..., (Vector 0 ipv5 ipv9) `cast` ... #)
                 }
                 }
                 }
                 }
             }
             }
             }
             }))
       `cast` ...
  of _ { Vector ipv4 ipv5 ipv6 ->
  case (imapD' $fDTInt $dDT2 lvl182 theGang lvl183 lvl184) `cast` ...
  of nt2 { DVector ipv7 ipv8 ipv9 ipv10 ->
  case (runSTRep
          (\ (@ s) (s :: State# s) ->
             case scanD $fDTInt lvl181 theGang $fNumInt_$c+ lvl13 ipv7
             of _ { (di, n) ->
             case di `cast` ... of nt3 { DInt ipv11 ->
             case n of n1 { I# ipv12 ->
             case >=# ipv12 0 of _ {
               False ->
                 case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv12)
                 of wild1 {
                 };
               True ->
                 case newByteArray# (*# ipv12 8) (s `cast` ...)
                 of _ { (# ipv13, ipv14 #) ->
                 case theGang of _ { Gang ww ww1 ww2 ->
                 let {
                   nt4 :: R:Dist(,) Int (Vector Double)
                   nt4 = DProd (nt3 `cast` ...) (nt2 `cast` ...) } in
                 case ($wa1
                         ww
                         ww1
                         ww2
                         (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                         (WorkCopy n1)
                         ((\ (i :: Int) (s1 :: State# s) ->
                             case ((((myD lvl180 (nt4 `cast` ...)) `cast` ...) i) `cast` ...) s1
                             of _ { (# ipv15, ipv16 #) ->
                             case ipv16 of _ { (x, y) ->
                             case x of _ { I# ipv17 ->
                             case y `cast` ... of _ { Vector ipv18 ipv19 ipv20 ->
                             case copyByteArray#
                                    ipv20
                                    (*# ipv18 8)
                                    ipv14
                                    (*# ipv17 8)
                                    (*# ipv19 8)
                                    (ipv15 `cast` ...)
                             of s'# { __DEFAULT ->
                             (# s'#, () #) `cast` ...
                             }
                             }
                             }
                             }
                             })
                          `cast` ...)
                         (ipv13 `cast` ...))
                      `cast` ...
                 of _ { (# ipv15, _ #) ->
                 case unsafeFreezeByteArray# ipv14 (ipv15 `cast` ...)
                 of _ { (# ipv17, ipv18 #) ->
                 (# ipv17 `cast` ..., (Vector 0 ipv12 ipv18) `cast` ... #)
                 }
                 }
                 }
                 }
             }
             }
             }
             }))
       `cast` ...
  of _ { Vector ipv11 ipv12 ipv13 ->
  case (imapD' $fDTInt $dDT2 lvl177 theGang lvl178 lvl179) `cast` ...
  of nt4 { DVector ipv14 ipv15 ipv16 ipv17 ->
  case (runSTRep
          (\ (@ s) (s :: State# s) ->
             case scanD $fDTInt lvl176 theGang $fNumInt_$c+ lvl13 ipv14
             of _ { (di, n) ->
             case di `cast` ... of nt5 { DInt ipv18 ->
             case n of n1 { I# ipv19 ->
             case >=# ipv19 0 of _ {
               False ->
                 case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv19)
                 of wild1 {
                 };
               True ->
                 case newByteArray# (*# ipv19 8) (s `cast` ...)
                 of _ { (# ipv20, ipv21 #) ->
                 case theGang of _ { Gang ww ww1 ww2 ->
                 let {
                   nt6 :: R:Dist(,) Int (Vector Double)
                   nt6 = DProd (nt5 `cast` ...) (nt4 `cast` ...) } in
                 case ($wa1
                         ww
                         ww1
                         ww2
                         (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                         (WorkCopy n1)
                         ((\ (i :: Int) (s1 :: State# s) ->
                             case ((((myD lvl175 (nt6 `cast` ...)) `cast` ...) i) `cast` ...) s1
                             of _ { (# ipv22, ipv23 #) ->
                             case ipv23 of _ { (x, y) ->
                             case x of _ { I# ipv24 ->
                             case y `cast` ... of _ { Vector ipv25 ipv26 ipv27 ->
                             case copyByteArray#
                                    ipv27
                                    (*# ipv25 8)
                                    ipv21
                                    (*# ipv24 8)
                                    (*# ipv26 8)
                                    (ipv22 `cast` ...)
                             of s'# { __DEFAULT ->
                             (# s'#, () #) `cast` ...
                             }
                             }
                             }
                             }
                             })
                          `cast` ...)
                         (ipv20 `cast` ...))
                      `cast` ...
                 of _ { (# ipv22, _ #) ->
                 case unsafeFreezeByteArray# ipv21 (ipv22 `cast` ...)
                 of _ { (# ipv24, ipv25 #) ->
                 (# ipv24 `cast` ..., (Vector 0 ipv19 ipv25) `cast` ... #)
                 }
                 }
                 }
                 }
             }
             }
             }
             }))
       `cast` ...
  of _ { Vector ipv18 ipv19 ipv20 ->
  case (imapD' $fDTInt $dDT2 lvl172 theGang lvl173 lvl174) `cast` ...
  of nt6 { DVector ipv21 ipv22 ipv23 ipv24 ->
  case (runSTRep
          (\ (@ s) (s :: State# s) ->
             case scanD $fDTInt lvl171 theGang $fNumInt_$c+ lvl13 ipv21
             of _ { (di, n) ->
             case di `cast` ... of nt7 { DInt ipv25 ->
             case n of n1 { I# ipv26 ->
             case >=# ipv26 0 of _ {
               False ->
                 case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv26)
                 of wild1 {
                 };
               True ->
                 case newByteArray# (*# ipv26 8) (s `cast` ...)
                 of _ { (# ipv27, ipv28 #) ->
                 case theGang of _ { Gang ww ww1 ww2 ->
                 let {
                   nt8 :: R:Dist(,) Int (Vector Double)
                   nt8 = DProd (nt7 `cast` ...) (nt6 `cast` ...) } in
                 case ($wa1
                         ww
                         ww1
                         ww2
                         (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                         (WorkCopy n1)
                         ((\ (i :: Int) (s1 :: State# s) ->
                             case ((((myD lvl170 (nt8 `cast` ...)) `cast` ...) i) `cast` ...) s1
                             of _ { (# ipv29, ipv30 #) ->
                             case ipv30 of _ { (x, y) ->
                             case x of _ { I# ipv31 ->
                             case y `cast` ... of _ { Vector ipv32 ipv33 ipv34 ->
                             case copyByteArray#
                                    ipv34
                                    (*# ipv32 8)
                                    ipv28
                                    (*# ipv31 8)
                                    (*# ipv33 8)
                                    (ipv29 `cast` ...)
                             of s'# { __DEFAULT ->
                             (# s'#, () #) `cast` ...
                             }
                             }
                             }
                             }
                             })
                          `cast` ...)
                         (ipv27 `cast` ...))
                      `cast` ...
                 of _ { (# ipv29, _ #) ->
                 case unsafeFreezeByteArray# ipv28 (ipv29 `cast` ...)
                 of _ { (# ipv31, ipv32 #) ->
                 (# ipv31 `cast` ..., (Vector 0 ipv26 ipv32) `cast` ... #)
                 }
                 }
                 }
                 }
             }
             }
             }
             }))
       `cast` ...
  of _ { Vector ipv25 ipv26 ipv27 ->
  case (imapD' $fDTInt $dDT2 lvl167 theGang lvl168 lvl169) `cast` ...
  of nt8 { DVector ipv28 ipv29 ipv30 ipv31 ->
  case (runSTRep
          (\ (@ s) (s :: State# s) ->
             case scanD $fDTInt lvl166 theGang $fNumInt_$c+ lvl13 ipv28
             of _ { (di, n) ->
             case di `cast` ... of nt9 { DInt ipv32 ->
             case n of n1 { I# ipv33 ->
             case >=# ipv33 0 of _ {
               False ->
                 case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv33)
                 of wild1 {
                 };
               True ->
                 case newByteArray# (*# ipv33 8) (s `cast` ...)
                 of _ { (# ipv34, ipv35 #) ->
                 case theGang of _ { Gang ww ww1 ww2 ->
                 let {
                   nt10 :: R:Dist(,) Int (Vector Double)
                   nt10 = DProd (nt9 `cast` ...) (nt8 `cast` ...) } in
                 case ($wa1
                         ww
                         ww1
                         ww2
                         (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                         (WorkCopy n1)
                         ((\ (i :: Int) (s1 :: State# s) ->
                             case ((((myD lvl165 (nt10 `cast` ...)) `cast` ...) i) `cast` ...)
                                    s1
                             of _ { (# ipv36, ipv37 #) ->
                             case ipv37 of _ { (x, y) ->
                             case x of _ { I# ipv38 ->
                             case y `cast` ... of _ { Vector ipv39 ipv40 ipv41 ->
                             case copyByteArray#
                                    ipv41
                                    (*# ipv39 8)
                                    ipv35
                                    (*# ipv38 8)
                                    (*# ipv40 8)
                                    (ipv36 `cast` ...)
                             of s'# { __DEFAULT ->
                             (# s'#, () #) `cast` ...
                             }
                             }
                             }
                             }
                             })
                          `cast` ...)
                         (ipv34 `cast` ...))
                      `cast` ...
                 of _ { (# ipv36, _ #) ->
                 case unsafeFreezeByteArray# ipv35 (ipv36 `cast` ...)
                 of _ { (# ipv38, ipv39 #) ->
                 (# ipv38 `cast` ..., (Vector 0 ipv33 ipv39) `cast` ... #)
                 }
                 }
                 }
                 }
             }
             }
             }
             }))
       `cast` ...
  of _ { Vector ipv32 ipv33 ipv34 ->
  case (imapD' $fDTInt $dDT2 lvl162 theGang lvl163 lvl164) `cast` ...
  of nt10 { DVector ipv35 ipv36 ipv37 ipv38 ->
  case (runSTRep
          (\ (@ s) (s :: State# s) ->
             case scanD $fDTInt lvl161 theGang $fNumInt_$c+ lvl13 ipv35
             of _ { (di, n) ->
             case di `cast` ... of nt11 { DInt ipv39 ->
             case n of n1 { I# ipv40 ->
             case >=# ipv40 0 of _ {
               False ->
                 case checkError lvl6 lvl7 Bounds lvl8 (checkLength_msg# ipv40)
                 of wild1 {
                 };
               True ->
                 case newByteArray# (*# ipv40 8) (s `cast` ...)
                 of _ { (# ipv41, ipv42 #) ->
                 case theGang of _ { Gang ww ww1 ww2 ->
                 let {
                   nt12 :: R:Dist(,) Int (Vector Double)
                   nt12 = DProd (nt11 `cast` ...) (nt10 `cast` ...) } in
                 case ($wa1
                         ww
                         ww1
                         ww2
                         (++ $fShowComp2 ($fShowComp_$s$cshowsPrec 11 (WJoinCopy n1) ([])))
                         (WorkCopy n1)
                         ((\ (i :: Int) (s1 :: State# s) ->
                             case ((((myD lvl160 (nt12 `cast` ...)) `cast` ...) i) `cast` ...)
                                    s1
                             of _ { (# ipv43, ipv44 #) ->
                             case ipv44 of _ { (x, y) ->
                             case x of _ { I# ipv45 ->
                             case y `cast` ... of _ { Vector ipv46 ipv47 ipv48 ->
                             case copyByteArray#
                                    ipv48
                                    (*# ipv46 8)
                                    ipv42
                                    (*# ipv45 8)
                                    (*# ipv47 8)
                                    (ipv43 `cast` ...)
                             of s'# { __DEFAULT ->
                             (# s'#, () #) `cast` ...
                             }
                             }
                             }
                             }
                             })
                          `cast` ...)
                         (ipv41 `cast` ...))
                      `cast` ...
                 of _ { (# ipv43, _ #) ->
                 case unsafeFreezeByteArray# ipv42 (ipv43 `cast` ...)
                 of _ { (# ipv45, ipv46 #) ->
                 (# ipv45 `cast` ..., (Vector 0 ipv40 ipv46) `cast` ... #)
                 }
                 }
                 }
                 }
             }
             }
             }
             }))
       `cast` ...
  of _ { Vector ipv39 ipv40 ipv41 ->
  runSTRep
    (\ (@ s) (s :: State# s) ->
       case newByteArray#
              (*# (+# ipv5 (+# ipv12 (+# ipv19 (+# ipv26 (+# ipv33 ipv40))))) 8)
              (s `cast` ...)
       of _ { (# ipv42, ipv43 #) ->
       letrec {
         $s$wa :: Int# -> Int# -> State# s -> (# State# s, Int #)
         $s$wa =
           \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
             case >=# sc1 ipv40 of _ {
               False ->
                 case indexDoubleArray# ipv41 (+# ipv39 sc1) of wild { __DEFAULT ->
                 case writeDoubleArray# ipv43 sc wild (sc2 `cast` ...)
                 of s'# { __DEFAULT ->
                 $s$wa (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                 }
                 };
               True -> (# sc2, I# sc #)
             }; } in
       letrec {
         $s$wa1 :: Int# -> Int# -> State# s -> (# State# s, Int #)
         $s$wa1 =
           \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
             case >=# sc1 ipv33 of _ {
               False ->
                 case indexDoubleArray# ipv34 (+# ipv32 sc1) of wild { __DEFAULT ->
                 case writeDoubleArray# ipv43 sc wild (sc2 `cast` ...)
                 of s'# { __DEFAULT ->
                 $s$wa1 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                 }
                 };
               True -> $s$wa sc 0 sc2
             }; } in
       letrec {
         $s$wa2 :: Int# -> Int# -> State# s -> (# State# s, Int #)
         $s$wa2 =
           \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
             case >=# sc1 ipv26 of _ {
               False ->
                 case indexDoubleArray# ipv27 (+# ipv25 sc1) of wild { __DEFAULT ->
                 case writeDoubleArray# ipv43 sc wild (sc2 `cast` ...)
                 of s'# { __DEFAULT ->
                 $s$wa2 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                 }
                 };
               True -> $s$wa1 sc 0 sc2
             }; } in
       letrec {
         $s$wa3 :: Int# -> Int# -> State# s -> (# State# s, Int #)
         $s$wa3 =
           \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
             case >=# sc1 ipv19 of _ {
               False ->
                 case indexDoubleArray# ipv20 (+# ipv18 sc1) of wild { __DEFAULT ->
                 case writeDoubleArray# ipv43 sc wild (sc2 `cast` ...)
                 of s'# { __DEFAULT ->
                 $s$wa3 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                 }
                 };
               True -> $s$wa2 sc 0 sc2
             }; } in
       letrec {
         $s$wa4 :: Int# -> Int# -> State# s -> (# State# s, Int #)
         $s$wa4 =
           \ (sc :: Int#) (sc1 :: Int#) (sc2 :: State# s) ->
             case >=# sc1 ipv12 of _ {
               False ->
                 case indexDoubleArray# ipv13 (+# ipv11 sc1) of wild { __DEFAULT ->
                 case writeDoubleArray# ipv43 sc wild (sc2 `cast` ...)
                 of s'# { __DEFAULT ->
                 $s$wa4 (+# sc 1) (+# sc1 1) (s'# `cast` ...)
                 }
                 };
               True -> $s$wa3 sc 0 sc2
             }; } in
       letrec {
         $s$wa5 :: Int# -> State# s -> Int# -> (# State# s, Int #)
         $s$wa5 =
           \ (sc :: Int#) (sc1 :: State# s) (sc2 :: Int#) ->
             case >=# sc2 ipv5 of _ {
               False ->
                 case indexDoubleArray# ipv6 (+# ipv4 sc2) of wild { __DEFAULT ->
                 case writeDoubleArray# ipv43 sc wild (sc1 `cast` ...)
                 of s'# { __DEFAULT ->
                 $s$wa5 (+# sc 1) (s'# `cast` ...) (+# sc2 1)
                 }
                 };
               True -> $s$wa4 sc 0 sc1
             }; } in
       case $s$wa5 0 (ipv42 `cast` ...) 0 of _ { (# ipv44, ipv45 #) ->
       case ipv45 of _ { I# tpl1 ->
       case unsafeFreezeByteArray# ipv43 (ipv44 `cast` ...)
       of _ { (# ipv46, ipv47 #) ->
       (# ipv46 `cast` ..., (Vector 0 tpl1 ipv47) `cast` ... #)
       }
       }
       }
       })
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }

$vexample7 :: R:PDataDouble
$vexample7 = PDouble $vexample8

$vexample6 :: PArray Double
$vexample6 = PArray 6 ($vexample7 `cast` ...)

$vexample :: Double
$vexample =
  case $wvsmvm 4 $vexample9 $vexample6 of _ { (# _, ww2 #) ->
  case ww2 `cast` ... of _ { PDouble xs ->
  case xs `cast` ... of _ { Vector ipv ipv1 ipv2 ->
  case theGang of wild1 { Gang rb ds1 ds2 ->
  case quotInt# ipv1 rb of wild2 { __DEFAULT ->
  case remInt# ipv1 rb of wild3 { __DEFAULT ->
  foldD
    $fDTDouble
    $vexample4
    wild1
    plusDouble
    (generateD
       $fDTDouble
       $vexample1
       wild1
       ((\ (i :: Int) ->
           case i of _ { I# x1 ->
           let {
             $w$j :: Int# -> Double#
             $w$j =
               \ (w :: Int#) ->
                 let {
                   $w$j1 :: Int# -> Double#
                   $w$j1 =
                     \ (w1 :: Int#) ->
                       let {
                         ipv3 :: Int#
                         ipv3 = +# ipv w } in
                       letrec {
                         $s$wfoldlM'_loop :: Double# -> Int# -> Double#
                         $s$wfoldlM'_loop =
                           \ (sc :: Double#) (sc1 :: Int#) ->
                             case >=# sc1 w1 of _ {
                               False ->
                                 case indexDoubleArray# ipv2 (+# ipv3 sc1) of wild6 { __DEFAULT ->
                                 $s$wfoldlM'_loop (+## sc wild6) (+# sc1 1)
                                 };
                               True -> sc
                             }; } in
                       $s$wfoldlM'_loop 0.0 0 } in
                 case <# x1 wild3 of _ {
                   False -> $w$j1 wild2;
                   True -> $w$j1 (+# wild2 1)
                 } } in
           case <# x1 wild3 of _ {
             False ->
               case $w$j (+# (*# wild2 x1) wild3) of ww3 { __DEFAULT ->
               (D# ww3) `cast` ...
               };
             True ->
               case $w$j (*# (+# wild2 1) x1) of ww3 { __DEFAULT ->
               (D# ww3) `cast` ...
               }
           }
           })
        `cast` ...))
  }
  }
  }
  }
  }
  }

example :: Double
example = $vexample



