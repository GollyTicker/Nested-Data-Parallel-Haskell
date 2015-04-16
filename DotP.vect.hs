
==================== Vectorisation ====================
Result size of Vectorisation
  = {terms: 431, types: 814, coercions: 236}

Rec { }
Rec {
dotpWrapper :: PArray Double -> PArray Double -> Double
dotpWrapper =
  \ (x :: PArray Double) (x :: PArray Double) ->
    $: ($: $vdotpWrapper x) x

$vdotpWrapper :: PArray Double :-> (PArray Double :-> Double)
$vdotpWrapper = closure $fPAVoid vdotpWrapper ldotpWrapper void

ldotpWrapper
  :: Int#
     -> PData Void
     -> PData (PArray Double)
     -> PData (PArray Double :-> Double)
ldotpWrapper =
  \ (lc :: Int#)
    (env :: PData Void)
    (arg :: PData (PArray Double)) ->
    ldotpWrapper lc arg

vdotpWrapper :: Void -> PArray Double -> PArray Double :-> Double
vdotpWrapper =
  \ (env :: Void) (arg :: PArray Double) -> vdotpWrapper arg

ldotpWrapper
  :: Int#
     -> PData (PArray Double) -> PData (PArray Double :-> Double)
ldotpWrapper =
  \ (lc :: Int#) (x :: PData (PArray Double)) ->
    liftedClosure
      ($fPAPArray
         (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
      vdotpWrapper
      ldotpWrapper
      x

vdotpWrapper :: PArray Double -> PArray Double :-> Double
vdotpWrapper =
  \ (x :: PArray Double) ->
    closure
      ($fPAPArray
         (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
      vdotpWrapper
      ldotpWrapper
      x

ldotpWrapper
  :: Int#
     -> PData (PArray Double) -> PData (PArray Double) -> PData Double
ldotpWrapper =
  \ (lc :: Int#)
    (env :: PData (PArray Double))
    (arg :: PData (PArray Double)) ->
    let {
      x :: PData (PArray Double)
      x = env } in
    ldotpWrapper lc x arg

vdotpWrapper :: PArray Double -> PArray Double -> Double
vdotpWrapper =
  \ (env :: PArray Double) (arg :: PArray Double) ->
    let {
      x :: PArray Double
      x = env } in
    vdotpWrapper x arg

ldotpWrapper
  :: Int#
     -> PData (PArray Double) -> PData (PArray Double) -> PData Double
ldotpWrapper =
  \ (lc :: Int#)
    (v :: PData (PArray Double))
    (w :: PData (PArray Double)) ->
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
                        (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPADouble)) `cast` ...)
                        $fPADouble
                        $fPADouble)
                     $fPADouble)
                  ($fPA:->
                     (($fPR:->) `cast` ...)
                     ($fPAPArray
                        (($fPRPArray
                            ($p1PA
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPADouble)) `cast` ...)
                                  $fPADouble
                                  $fPADouble)))
                         `cast` ...)
                        ($fPA(,)
                           (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPADouble)) `cast` ...)
                           $fPADouble
                           $fPADouble))
                     ($fPAPArray
                        (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)))
               lc
               ($vmapP
                  ($fPA(,)
                     (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPADouble)) `cast` ...)
                     $fPADouble
                     $fPADouble)
                  $fPADouble))
            (liftedClosure $fPAVoid vdotpWrapper ldotpWrapper pvoid))
         (liftedApply
            lc
            (liftedApply
               lc
               (replicatePD
                  ($fPA:->
                     (($fPR:->) `cast` ...)
                     ($fPAPArray
                        (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
                     ($fPA:->
                        (($fPR:->) `cast` ...)
                        ($fPAPArray
                           (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
                        ($fPAPArray
                           (($fPRPArray
                               ($p1PA
                                  ($fPA(,)
                                     (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPADouble))
                                      `cast` ...)
                                     $fPADouble
                                     $fPADouble)))
                            `cast` ...)
                           ($fPA(,)
                              (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPADouble)) `cast` ...)
                              $fPADouble
                              $fPADouble))))
                  lc
                  ($vzipP $fPADouble $fPADouble))
               (liftedApply
                  lc
                  (liftedApply
                     lc
                     (replicatePD
                        ($fPA:->
                           (($fPR:->) `cast` ...)
                           ($fPA:-> (($fPR:->) `cast` ...) $fPADouble $fPADouble)
                           ($fPA:->
                              (($fPR:->) `cast` ...)
                              ($fPAPArray
                                 (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
                              ($fPAPArray
                                 (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)))
                        lc
                        ($vmapP $fPADouble $fPADouble))
                     (liftedClosure $fPAVoid vdotpWrapper ldotpWrapper pvoid))
                  (liftedApply
                     lc
                     (replicatePD
                        ($fPA:->
                           (($fPR:->) `cast` ...)
                           ($fPAPArray
                              (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
                           ($fPAPArray
                              (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble))
                        lc
                        ($vfromPArrayP $fPADouble))
                     v)))
            (liftedApply
               lc
               (liftedApply
                  lc
                  (replicatePD
                     ($fPA:->
                        (($fPR:->) `cast` ...)
                        ($fPA:-> (($fPR:->) `cast` ...) $fPADouble $fPADouble)
                        ($fPA:->
                           (($fPR:->) `cast` ...)
                           ($fPAPArray
                              (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
                           ($fPAPArray
                              (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)))
                     lc
                     ($vmapP $fPADouble $fPADouble))
                  (liftedClosure $fPAVoid vdotpWrapper ldotpWrapper pvoid))
               (liftedApply
                  lc
                  (replicatePD
                     ($fPA:->
                        (($fPR:->) `cast` ...)
                        ($fPAPArray
                           (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble)
                        ($fPAPArray
                           (($fPRPArray ($p1PA $fPADouble)) `cast` ...) $fPADouble))
                     lc
                     ($vfromPArrayP $fPADouble))
                  w))))

vdotpWrapper :: PArray Double -> PArray Double -> Double
vdotpWrapper =
  \ (v :: PArray Double) (w :: PArray Double) ->
    $:
      $vsumP
      ($:
         ($:
            ($vmapP
               ($fPA(,)
                  (($fPR(,) ($fPRWrap $fPADouble) ($fPRWrap $fPADouble)) `cast` ...)
                  $fPADouble
                  $fPADouble)
               $fPADouble)
            (closure $fPAVoid vdotpWrapper ldotpWrapper void))
         ($:
            ($:
               ($vzipP $fPADouble $fPADouble)
               ($:
                  ($:
                     ($vmapP $fPADouble $fPADouble)
                     (closure $fPAVoid vdotpWrapper ldotpWrapper void))
                  ($: ($vfromPArrayP $fPADouble) v)))
            ($:
               ($:
                  ($vmapP $fPADouble $fPADouble)
                  (closure $fPAVoid vdotpWrapper ldotpWrapper void))
               ($: ($vfromPArrayP $fPADouble) w))))

ldotpWrapper :: Int# -> PData Void -> PData Double -> PData Double
ldotpWrapper =
  \ (lc :: Int#) (env :: PData Void) (arg :: PData Double) ->
    ldotpWrapper lc arg

vdotpWrapper :: Void -> Double -> Double
vdotpWrapper = \ (env :: Void) (arg :: Double) -> vdotpWrapper arg

ldotpWrapper :: Int# -> PData Double -> PData Double
ldotpWrapper = \ (lc :: Int#) (ds :: PData Double) -> ds

vdotpWrapper :: Double -> Double
vdotpWrapper = \ (ds :: Double) -> ds

ldotpWrapper :: Int# -> PData Void -> PData Double -> PData Double
ldotpWrapper =
  \ (lc :: Int#) (env :: PData Void) (arg :: PData Double) ->
    ldotpWrapper lc arg

vdotpWrapper :: Void -> Double -> Double
vdotpWrapper = \ (env :: Void) (arg :: Double) -> vdotpWrapper arg

ldotpWrapper :: Int# -> PData Double -> PData Double
ldotpWrapper = \ (lc :: Int#) (ds :: PData Double) -> ds

vdotpWrapper :: Double -> Double
vdotpWrapper = \ (ds :: Double) -> ds

ldotpWrapper
  :: Int# -> PData Void -> PData (Double, Double) -> PData Double
ldotpWrapper =
  \ (lc :: Int#)
    (env :: PData Void)
    (arg :: PData (Double, Double)) ->
    ldotpWrapper lc arg

vdotpWrapper :: Void -> (Double, Double) -> Double
vdotpWrapper =
  \ (env :: Void) (arg :: (Double, Double)) -> vdotpWrapper arg

ldotpWrapper :: Int# -> PData (Double, Double) -> PData Double
ldotpWrapper =
  \ (lc :: Int#) (ds :: PData (Double, Double)) ->
    let {
      scrut :: PData (Double, Double)
      scrut = ds } in
    case scrut `cast` ... of wild { PTuple2 x y ->
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
         x)
      y
    }

vdotpWrapper :: (Double, Double) -> Double
vdotpWrapper =
  \ (ds :: (Double, Double)) ->
    let {
      scrut :: (Double, Double)
      scrut = ds } in
    case scrut of wild { (x, y) -> $: ($: $v* x) y }
end Rec }



