
==================== Vectorisation ====================
Result size of Vectorisation
  = {terms: 1,712, types: 3,101, coercions: 1,216}

Rec {
$paPreds :: PA Preds
$paPreds =
  D:PA
    (($fPRSum2 $fPRVoid ($fPR(,) $fPRInt ($fPRWrap $paPreds)))
     `cast` ...)
    $paPreds$toPRepr
    $paPreds$fromPRepr
    $paPreds$toArrPRepr
    $paPreds$fromArrPRepr
    $paPreds$toArrPReprs
    $paPreds$fromArrPReprs

$paPreds$fromArrPReprs :: PDatas (PRepr Preds) -> PDatas Preds
$paPreds$fromArrPReprs =
  \ (xss :: PDatas (PRepr Preds)) ->
    case xss `cast` ... of wild { PSum2s sels xs xs ->
    case xs `cast` ... of wild { PTuple2s ys ys ->
    (VPDs:Preds sels ys (ys `cast` ...)) `cast` ...
    }
    }

$paPreds$toArrPReprs :: PDatas Preds -> PDatas (PRepr Preds)
$paPreds$toArrPReprs =
  \ (xss :: PDatas Preds) ->
    case xss `cast` ... of wild { VPDs:Preds sels x x ->
    (PSum2s
       sels
       (pvoids# (lengthSels2# sels))
       ((PTuple2s x (x `cast` ...)) `cast` ...))
    `cast` ...
    }

$paPreds$fromArrPRepr :: PData (PRepr Preds) -> PData Preds
$paPreds$fromArrPRepr =
  \ (xs :: PData (PRepr Preds)) ->
    case xs `cast` ... of wild { PSum2 sel xs xs ->
    case xs `cast` ... of wild { PTuple2 ys ys ->
    (VPD:Preds sel ys (ys `cast` ...)) `cast` ...
    }
    }

$paPreds$toArrPRepr :: PData Preds -> PData (PRepr Preds)
$paPreds$toArrPRepr =
  \ (xs :: PData Preds) ->
    case xs `cast` ... of wild { VPD:Preds sel x x ->
    (PSum2 sel pvoid ((PTuple2 x (x `cast` ...)) `cast` ...))
    `cast` ...
    }

$paPreds$fromPRepr :: PRepr Preds -> Preds
$paPreds$fromPRepr =
  \ (x :: PRepr Preds) ->
    case x `cast` ... of wild {
      Alt2_1 x -> Nil;
      Alt2_2 x -> case x of wild { (y, y) -> Cons y (y `cast` ...) }
    }

$paPreds$toPRepr :: Preds -> PRepr Preds
$paPreds$toPRepr =
  \ (x :: Preds) ->
    case x of wild {
      Nil -> (Alt2_1 void) `cast` ...;
      Cons x x -> (Alt2_2 (x, x `cast` ...)) `cast` ...
    }

$vCons :: Pred :-> (Preds :-> Preds)
$vCons = closure $fPAVoid vCons lCons void

$vNil :: Preds
$vNil = Nil

lCons
  :: Int# -> PData Void -> PData Pred -> PData (Preds :-> Preds)
lCons =
  \ (lc :: Int#) (env :: PData Void) (arg :: PData Pred) ->
    lCons lc arg

vCons :: Void -> Pred -> Preds :-> Preds
vCons = \ (env :: Void) (arg :: Pred) -> vCons arg

lCons :: Int# -> PData Pred -> PData (Preds :-> Preds)
lCons =
  \ (lc :: Int#) (x :: PData Pred) ->
    liftedClosure $fPAInt vCons lCons x

vCons :: Pred -> Preds :-> Preds
vCons = \ (x :: Pred) -> closure $fPAInt vCons lCons x

lCons :: Int# -> PData Pred -> PData Preds -> PData Preds
lCons =
  \ (lc :: Int#) (env :: PData Pred) (arg :: PData Preds) ->
    let {
      x :: PData Pred
      x = env } in
    (\ (lc :: Int#) (xs :: PData Pred) (xs :: PData Preds) ->
       (VPD:Preds (replicateSel2# lc 1) xs xs) `cast` ...)
      lc x arg

vCons :: Pred -> Preds -> Preds
vCons =
  \ (env :: Pred) (arg :: Preds) ->
    let {
      x :: Pred
      x = env } in
    Cons x arg
end Rec }

Rec {
minBySndP :: [:(Preds, Int):] -> (Preds, Int)
minBySndP =
  \ (ps :: [:(Preds, Int):]) ->
    let {
      pivot :: Int
      pivot = div (lengthP ps) (I# 2) } in
    let {
      minBoth :: [:(Preds, Int):]
      minBoth =
        mapP
          (\ (ds :: [:(Preds, Int):]) -> minBySndP ds)
          (+:+
             (singletonP (sliceP (I# 0) pivot ps))
             (singletonP
                (sliceP (+ pivot (I# 1)) (- (lengthP ps) (I# 1)) ps))) } in
    let {
      minL :: (Preds, Int)
      minL = !: minBoth (I# 0) } in
    let {
      minR :: (Preds, Int)
      minR = !: minBoth (I# 1) } in
    let {
      f :: (Preds, Int) -> Int
      f = \ (ds :: (Preds, Int)) -> case ds of _ { (a, b) -> b } } in
    case < (lengthP ps) (I# 2) of _ {
      False ->
        case < (f minL) (f minR) of _ {
          False -> minR;
          True -> minL
        };
      True -> !: ps (I# 0)
    }

$vminBySndP :: V:GHC:PArr_[::] (Preds, Int) :-> (Preds, Int)
$vminBySndP = closure $fPAVoid vminBySndP lminBySndP void

lminBySndP
  :: Int#
     -> PData Void
     -> PData (V:GHC:PArr_[::] (Preds, Int))
     -> PData (Preds, Int)
lminBySndP =
  \ (lc :: Int#)
    (env :: PData Void)
    (arg :: PData (V:GHC:PArr_[::] (Preds, Int))) ->
    lminBySndP lc arg

vminBySndP :: Void -> V:GHC:PArr_[::] (Preds, Int) -> (Preds, Int)
vminBySndP =
  \ (env :: Void) (arg :: V:GHC:PArr_[::] (Preds, Int)) ->
    vminBySndP arg

lminBySndP
  :: Int#
     -> PData (V:GHC:PArr_[::] (Preds, Int)) -> PData (Preds, Int)
lminBySndP =
  \ (lc :: Int#) (ps :: PData (V:GHC:PArr_[::] (Preds, Int))) ->
    case lc of wild {
      __DEFAULT ->
        let {
          pivot :: PData Int
          pivot =
            liftedApply
              lc
              (liftedApply
                 lc
                 (replicatePD
                    ($fPA:->
                       (($fPR:->) `cast` ...)
                       $fPAInt
                       ($fPA:-> (($fPR:->) `cast` ...) $fPAInt $fPAInt))
                    lc
                    $vdiv)
                 (liftedApply
                    lc
                    (replicatePD
                       ($fPA:->
                          (($fPR:->) `cast` ...)
                          ($fPAPArray
                             (($fPRPArray
                                 ($p1PA
                                    ($fPA(,)
                                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                       $paPreds
                                       $fPAInt)))
                              `cast` ...)
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                $paPreds
                                $fPAInt))
                          $fPAInt)
                       lc
                       ($vlengthP
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                             $paPreds
                             $fPAInt)))
                    ps))
              (replicatePD $fPAInt lc (I# 2)) } in
        let {
          minBoth :: PData (V:GHC:PArr_[::] (Preds, Int))
          minBoth =
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
                                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                       $paPreds
                                       $fPAInt)))
                              `cast` ...)
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                $paPreds
                                $fPAInt))
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                             $paPreds
                             $fPAInt))
                       ($fPA:->
                          (($fPR:->) `cast` ...)
                          ($fPAPArray
                             (($fPRPArray
                                 ($p1PA
                                    ($fPAPArray
                                       (($fPRPArray
                                           ($p1PA
                                              ($fPA(,)
                                                 (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                  `cast` ...)
                                                 $paPreds
                                                 $fPAInt)))
                                        `cast` ...)
                                       ($fPA(,)
                                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                           `cast` ...)
                                          $paPreds
                                          $fPAInt))))
                              `cast` ...)
                             ($fPAPArray
                                (($fPRPArray
                                    ($p1PA
                                       ($fPA(,)
                                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                           `cast` ...)
                                          $paPreds
                                          $fPAInt)))
                                 `cast` ...)
                                ($fPA(,)
                                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                   $paPreds
                                   $fPAInt)))
                          ($fPAPArray
                             (($fPRPArray
                                 ($p1PA
                                    ($fPA(,)
                                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                       $paPreds
                                       $fPAInt)))
                              `cast` ...)
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                $paPreds
                                $fPAInt))))
                    lc
                    ($vmapP
                       ($fPAPArray
                          (($fPRPArray
                              ($p1PA
                                 ($fPA(,)
                                    (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                    $paPreds
                                    $fPAInt)))
                           `cast` ...)
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                             $paPreds
                             $fPAInt))
                       ($fPA(,)
                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                          $paPreds
                          $fPAInt)))
                 (liftedClosure $fPAVoid vminBoth lminBoth pvoid))
              (liftedApply
                 lc
                 (liftedApply
                    lc
                    (replicatePD
                       ($fPA:->
                          (($fPR:->) `cast` ...)
                          ($fPAPArray
                             (($fPRPArray
                                 ($p1PA
                                    ($fPAPArray
                                       (($fPRPArray
                                           ($p1PA
                                              ($fPA(,)
                                                 (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                  `cast` ...)
                                                 $paPreds
                                                 $fPAInt)))
                                        `cast` ...)
                                       ($fPA(,)
                                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                           `cast` ...)
                                          $paPreds
                                          $fPAInt))))
                              `cast` ...)
                             ($fPAPArray
                                (($fPRPArray
                                    ($p1PA
                                       ($fPA(,)
                                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                           `cast` ...)
                                          $paPreds
                                          $fPAInt)))
                                 `cast` ...)
                                ($fPA(,)
                                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                   $paPreds
                                   $fPAInt)))
                          ($fPA:->
                             (($fPR:->) `cast` ...)
                             ($fPAPArray
                                (($fPRPArray
                                    ($p1PA
                                       ($fPAPArray
                                          (($fPRPArray
                                              ($p1PA
                                                 ($fPA(,)
                                                    (($fPR(,)
                                                        ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                     `cast` ...)
                                                    $paPreds
                                                    $fPAInt)))
                                           `cast` ...)
                                          ($fPA(,)
                                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                              `cast` ...)
                                             $paPreds
                                             $fPAInt))))
                                 `cast` ...)
                                ($fPAPArray
                                   (($fPRPArray
                                       ($p1PA
                                          ($fPA(,)
                                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                              `cast` ...)
                                             $paPreds
                                             $fPAInt)))
                                    `cast` ...)
                                   ($fPA(,)
                                      (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                      $paPreds
                                      $fPAInt)))
                             ($fPAPArray
                                (($fPRPArray
                                    ($p1PA
                                       ($fPAPArray
                                          (($fPRPArray
                                              ($p1PA
                                                 ($fPA(,)
                                                    (($fPR(,)
                                                        ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                     `cast` ...)
                                                    $paPreds
                                                    $fPAInt)))
                                           `cast` ...)
                                          ($fPA(,)
                                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                              `cast` ...)
                                             $paPreds
                                             $fPAInt))))
                                 `cast` ...)
                                ($fPAPArray
                                   (($fPRPArray
                                       ($p1PA
                                          ($fPA(,)
                                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                              `cast` ...)
                                             $paPreds
                                             $fPAInt)))
                                    `cast` ...)
                                   ($fPA(,)
                                      (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                      $paPreds
                                      $fPAInt)))))
                       lc
                       ($v+:+
                          ($fPAPArray
                             (($fPRPArray
                                 ($p1PA
                                    ($fPA(,)
                                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                       $paPreds
                                       $fPAInt)))
                              `cast` ...)
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                $paPreds
                                $fPAInt))))
                    (liftedApply
                       lc
                       (replicatePD
                          ($fPA:->
                             (($fPR:->) `cast` ...)
                             ($fPAPArray
                                (($fPRPArray
                                    ($p1PA
                                       ($fPA(,)
                                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                           `cast` ...)
                                          $paPreds
                                          $fPAInt)))
                                 `cast` ...)
                                ($fPA(,)
                                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                   $paPreds
                                   $fPAInt))
                             ($fPAPArray
                                (($fPRPArray
                                    ($p1PA
                                       ($fPAPArray
                                          (($fPRPArray
                                              ($p1PA
                                                 ($fPA(,)
                                                    (($fPR(,)
                                                        ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                     `cast` ...)
                                                    $paPreds
                                                    $fPAInt)))
                                           `cast` ...)
                                          ($fPA(,)
                                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                              `cast` ...)
                                             $paPreds
                                             $fPAInt))))
                                 `cast` ...)
                                ($fPAPArray
                                   (($fPRPArray
                                       ($p1PA
                                          ($fPA(,)
                                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                              `cast` ...)
                                             $paPreds
                                             $fPAInt)))
                                    `cast` ...)
                                   ($fPA(,)
                                      (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                      $paPreds
                                      $fPAInt))))
                          lc
                          ($vsingletonP
                             ($fPAPArray
                                (($fPRPArray
                                    ($p1PA
                                       ($fPA(,)
                                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                           `cast` ...)
                                          $paPreds
                                          $fPAInt)))
                                 `cast` ...)
                                ($fPA(,)
                                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                   $paPreds
                                   $fPAInt))))
                       (liftedApply
                          lc
                          (liftedApply
                             lc
                             (liftedApply
                                lc
                                (replicatePD
                                   ($fPA:->
                                      (($fPR:->) `cast` ...)
                                      $fPAInt
                                      ($fPA:->
                                         (($fPR:->) `cast` ...)
                                         $fPAInt
                                         ($fPA:->
                                            (($fPR:->) `cast` ...)
                                            ($fPAPArray
                                               (($fPRPArray
                                                   ($p1PA
                                                      ($fPA(,)
                                                         (($fPR(,)
                                                             ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                          `cast` ...)
                                                         $paPreds
                                                         $fPAInt)))
                                                `cast` ...)
                                               ($fPA(,)
                                                  (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                   `cast` ...)
                                                  $paPreds
                                                  $fPAInt))
                                            ($fPAPArray
                                               (($fPRPArray
                                                   ($p1PA
                                                      ($fPA(,)
                                                         (($fPR(,)
                                                             ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                          `cast` ...)
                                                         $paPreds
                                                         $fPAInt)))
                                                `cast` ...)
                                               ($fPA(,)
                                                  (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                   `cast` ...)
                                                  $paPreds
                                                  $fPAInt)))))
                                   lc
                                   ($vsliceP
                                      ($fPA(,)
                                         (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                          `cast` ...)
                                         $paPreds
                                         $fPAInt)))
                                (replicatePD $fPAInt lc (I# 0)))
                             pivot)
                          ps)))
                 (liftedApply
                    lc
                    (replicatePD
                       ($fPA:->
                          (($fPR:->) `cast` ...)
                          ($fPAPArray
                             (($fPRPArray
                                 ($p1PA
                                    ($fPA(,)
                                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                       $paPreds
                                       $fPAInt)))
                              `cast` ...)
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                $paPreds
                                $fPAInt))
                          ($fPAPArray
                             (($fPRPArray
                                 ($p1PA
                                    ($fPAPArray
                                       (($fPRPArray
                                           ($p1PA
                                              ($fPA(,)
                                                 (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                  `cast` ...)
                                                 $paPreds
                                                 $fPAInt)))
                                        `cast` ...)
                                       ($fPA(,)
                                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                           `cast` ...)
                                          $paPreds
                                          $fPAInt))))
                              `cast` ...)
                             ($fPAPArray
                                (($fPRPArray
                                    ($p1PA
                                       ($fPA(,)
                                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                           `cast` ...)
                                          $paPreds
                                          $fPAInt)))
                                 `cast` ...)
                                ($fPA(,)
                                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                   $paPreds
                                   $fPAInt))))
                       lc
                       ($vsingletonP
                          ($fPAPArray
                             (($fPRPArray
                                 ($p1PA
                                    ($fPA(,)
                                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                       $paPreds
                                       $fPAInt)))
                              `cast` ...)
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                $paPreds
                                $fPAInt))))
                    (liftedApply
                       lc
                       (liftedApply
                          lc
                          (liftedApply
                             lc
                             (replicatePD
                                ($fPA:->
                                   (($fPR:->) `cast` ...)
                                   $fPAInt
                                   ($fPA:->
                                      (($fPR:->) `cast` ...)
                                      $fPAInt
                                      ($fPA:->
                                         (($fPR:->) `cast` ...)
                                         ($fPAPArray
                                            (($fPRPArray
                                                ($p1PA
                                                   ($fPA(,)
                                                      (($fPR(,)
                                                          ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                       `cast` ...)
                                                      $paPreds
                                                      $fPAInt)))
                                             `cast` ...)
                                            ($fPA(,)
                                               (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                `cast` ...)
                                               $paPreds
                                               $fPAInt))
                                         ($fPAPArray
                                            (($fPRPArray
                                                ($p1PA
                                                   ($fPA(,)
                                                      (($fPR(,)
                                                          ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                       `cast` ...)
                                                      $paPreds
                                                      $fPAInt)))
                                             `cast` ...)
                                            ($fPA(,)
                                               (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                `cast` ...)
                                               $paPreds
                                               $fPAInt)))))
                                lc
                                ($vsliceP
                                   ($fPA(,)
                                      (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                      $paPreds
                                      $fPAInt)))
                             (liftedApply
                                lc
                                (liftedApply
                                   lc
                                   (replicatePD
                                      ($fPA:->
                                         (($fPR:->) `cast` ...)
                                         $fPAInt
                                         ($fPA:-> (($fPR:->) `cast` ...) $fPAInt $fPAInt))
                                      lc
                                      $v+)
                                   pivot)
                                (replicatePD $fPAInt lc (I# 1))))
                          (liftedApply
                             lc
                             (liftedApply
                                lc
                                (replicatePD
                                   ($fPA:->
                                      (($fPR:->) `cast` ...)
                                      $fPAInt
                                      ($fPA:-> (($fPR:->) `cast` ...) $fPAInt $fPAInt))
                                   lc
                                   $v-)
                                (liftedApply
                                   lc
                                   (replicatePD
                                      ($fPA:->
                                         (($fPR:->) `cast` ...)
                                         ($fPAPArray
                                            (($fPRPArray
                                                ($p1PA
                                                   ($fPA(,)
                                                      (($fPR(,)
                                                          ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                       `cast` ...)
                                                      $paPreds
                                                      $fPAInt)))
                                             `cast` ...)
                                            ($fPA(,)
                                               (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                                `cast` ...)
                                               $paPreds
                                               $fPAInt))
                                         $fPAInt)
                                      lc
                                      ($vlengthP
                                         ($fPA(,)
                                            (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt))
                                             `cast` ...)
                                            $paPreds
                                            $fPAInt)))
                                   ps))
                             (replicatePD $fPAInt lc (I# 1))))
                       ps))) } in
        let {
          minL :: PData (Preds, Int)
          minL =
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
                                    (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                    $paPreds
                                    $fPAInt)))
                           `cast` ...)
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                             $paPreds
                             $fPAInt))
                       ($fPA:->
                          (($fPR:->) `cast` ...)
                          $fPAInt
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                             $paPreds
                             $fPAInt)))
                    lc
                    ($v!:
                       ($fPA(,)
                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                          $paPreds
                          $fPAInt)))
                 minBoth)
              (replicatePD $fPAInt lc (I# 0)) } in
        let {
          minR :: PData (Preds, Int)
          minR =
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
                                    (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                    $paPreds
                                    $fPAInt)))
                           `cast` ...)
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                             $paPreds
                             $fPAInt))
                       ($fPA:->
                          (($fPR:->) `cast` ...)
                          $fPAInt
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                             $paPreds
                             $fPAInt)))
                    lc
                    ($v!:
                       ($fPA(,)
                          (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                          $paPreds
                          $fPAInt)))
                 minBoth)
              (replicatePD $fPAInt lc (I# 1)) } in
        let {
          f :: PData ((Preds, Int) :-> Int)
          f = liftedClosure $fPAVoid vf lf pvoid } in
        let {
          scrut :: PData Bool
          scrut =
            liftedApply
              lc
              (liftedApply
                 lc
                 (replicatePD
                    ($fPA:->
                       (($fPR:->) `cast` ...)
                       $fPAInt
                       ($fPA:-> (($fPR:->) `cast` ...) $fPAInt $fPABool))
                    lc
                    $v<)
                 (liftedApply
                    lc
                    (replicatePD
                       ($fPA:->
                          (($fPR:->) `cast` ...)
                          ($fPAPArray
                             (($fPRPArray
                                 ($p1PA
                                    ($fPA(,)
                                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                       $paPreds
                                       $fPAInt)))
                              `cast` ...)
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                $paPreds
                                $fPAInt))
                          $fPAInt)
                       lc
                       ($vlengthP
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                             $paPreds
                             $fPAInt)))
                    ps))
              (replicatePD $fPAInt lc (I# 2)) } in
        case scrut `cast` ... of vv { PBool sel ->
        combine2PD
          ($fPA(,)
             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
             $paPreds
             $fPAInt)
          lc
          sel
          (case elementsSel2_0# sel of lc { __DEFAULT ->
           let {
             f :: PData ((Preds, Int) :-> Int)
             f =
               packByTagPD
                 ($fPA:->
                    (($fPR:->) `cast` ...)
                    ($fPA(,)
                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                       $paPreds
                       $fPAInt)
                    $fPAInt)
                 f
                 lc
                 (tagsSel2 sel)
                 0 } in
           let {
             minL :: PData (Preds, Int)
             minL =
               packByTagPD
                 ($fPA(,)
                    (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                    $paPreds
                    $fPAInt)
                 minL
                 lc
                 (tagsSel2 sel)
                 0 } in
           let {
             minR :: PData (Preds, Int)
             minR =
               packByTagPD
                 ($fPA(,)
                    (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                    $paPreds
                    $fPAInt)
                 minR
                 lc
                 (tagsSel2 sel)
                 0 } in
           let {
             scrut :: PData Bool
             scrut =
               liftedApply
                 lc
                 (liftedApply
                    lc
                    (replicatePD
                       ($fPA:->
                          (($fPR:->) `cast` ...)
                          $fPAInt
                          ($fPA:-> (($fPR:->) `cast` ...) $fPAInt $fPABool))
                       lc
                       $v<)
                    (liftedApply lc f minL))
                 (liftedApply lc f minR) } in
           case scrut `cast` ... of vv { PBool sel ->
           combine2PD
             ($fPA(,)
                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                $paPreds
                $fPAInt)
             lc
             sel
             (case elementsSel2_0# sel of lc { __DEFAULT ->
              let {
                minR :: PData (Preds, Int)
                minR =
                  packByTagPD
                    ($fPA(,)
                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                       $paPreds
                       $fPAInt)
                    minR
                    lc
                    (tagsSel2 sel)
                    0 } in
              minR
              })
             (case elementsSel2_1# sel of lc { __DEFAULT ->
              let {
                minL :: PData (Preds, Int)
                minL =
                  packByTagPD
                    ($fPA(,)
                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                       $paPreds
                       $fPAInt)
                    minL
                    lc
                    (tagsSel2 sel)
                    1 } in
              minL
              })
           }
           })
          (case elementsSel2_1# sel of lc { __DEFAULT ->
           let {
             ps :: PData (V:GHC:PArr_[::] (Preds, Int))
             ps =
               packByTagPD
                 ($fPAPArray
                    (($fPRPArray
                        ($p1PA
                           ($fPA(,)
                              (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                              $paPreds
                              $fPAInt)))
                     `cast` ...)
                    ($fPA(,)
                       (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                       $paPreds
                       $fPAInt))
                 ps
                 lc
                 (tagsSel2 sel)
                 1 } in
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
                                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                   $paPreds
                                   $fPAInt)))
                          `cast` ...)
                         ($fPA(,)
                            (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                            $paPreds
                            $fPAInt))
                      ($fPA:->
                         (($fPR:->) `cast` ...)
                         $fPAInt
                         ($fPA(,)
                            (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                            $paPreds
                            $fPAInt)))
                   lc
                   ($v!:
                      ($fPA(,)
                         (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                         $paPreds
                         $fPAInt)))
                ps)
             (replicatePD $fPAInt lc (I# 0))
           })
        };
      0 ->
        emptyPD
          ($fPA(,)
             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
             $paPreds
             $fPAInt)
    }

vminBySndP :: V:GHC:PArr_[::] (Preds, Int) -> (Preds, Int)
vminBySndP =
  \ (ps :: V:GHC:PArr_[::] (Preds, Int)) ->
    let {
      pivot :: Int
      pivot =
        $:
          ($:
             $vdiv
             ($:
                ($vlengthP
                   ($fPA(,)
                      (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                      $paPreds
                      $fPAInt))
                ps))
          (I# 2) } in
    let {
      minBoth :: V:GHC:PArr_[::] (Preds, Int)
      minBoth =
        $:
          ($:
             ($vmapP
                ($fPAPArray
                   (($fPRPArray
                       ($p1PA
                          ($fPA(,)
                             (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                             $paPreds
                             $fPAInt)))
                    `cast` ...)
                   ($fPA(,)
                      (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                      $paPreds
                      $fPAInt))
                ($fPA(,)
                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                   $paPreds
                   $fPAInt))
             (closure $fPAVoid vminBoth lminBoth void))
          ($:
             ($:
                ($v+:+
                   ($fPAPArray
                      (($fPRPArray
                          ($p1PA
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                $paPreds
                                $fPAInt)))
                       `cast` ...)
                      ($fPA(,)
                         (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                         $paPreds
                         $fPAInt)))
                ($:
                   ($vsingletonP
                      ($fPAPArray
                         (($fPRPArray
                             ($p1PA
                                ($fPA(,)
                                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                   $paPreds
                                   $fPAInt)))
                          `cast` ...)
                         ($fPA(,)
                            (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                            $paPreds
                            $fPAInt)))
                   ($:
                      ($:
                         ($:
                            ($vsliceP
                               ($fPA(,)
                                  (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                  $paPreds
                                  $fPAInt))
                            (I# 0))
                         pivot)
                      ps)))
             ($:
                ($vsingletonP
                   ($fPAPArray
                      (($fPRPArray
                          ($p1PA
                             ($fPA(,)
                                (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                $paPreds
                                $fPAInt)))
                       `cast` ...)
                      ($fPA(,)
                         (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                         $paPreds
                         $fPAInt)))
                ($:
                   ($:
                      ($:
                         ($vsliceP
                            ($fPA(,)
                               (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                               $paPreds
                               $fPAInt))
                         ($: ($: $v+ pivot) (I# 1)))
                      ($:
                         ($:
                            $v-
                            ($:
                               ($vlengthP
                                  ($fPA(,)
                                     (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                                     $paPreds
                                     $fPAInt))
                               ps))
                         (I# 1)))
                   ps))) } in
    let {
      minL :: (Preds, Int)
      minL =
        $:
          ($:
             ($v!:
                ($fPA(,)
                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                   $paPreds
                   $fPAInt))
             minBoth)
          (I# 0) } in
    let {
      minR :: (Preds, Int)
      minR =
        $:
          ($:
             ($v!:
                ($fPA(,)
                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                   $paPreds
                   $fPAInt))
             minBoth)
          (I# 1) } in
    let {
      f :: (Preds, Int) :-> Int
      f = closure $fPAVoid vf lf void } in
    let {
      scrut :: Bool
      scrut =
        $:
          ($:
             $v<
             ($:
                ($vlengthP
                   ($fPA(,)
                      (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                      $paPreds
                      $fPAInt))
                ps))
          (I# 2) } in
    case scrut of vv {
      False ->
        let {
          scrut :: Bool
          scrut = $: ($: $v< ($: f minL)) ($: f minR) } in
        case scrut of vv {
          False -> minR;
          True -> minL
        };
      True ->
        $:
          ($:
             ($v!:
                ($fPA(,)
                   (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                   $paPreds
                   $fPAInt))
             ps)
          (I# 0)
    }

lf :: Int# -> PData Void -> PData (Preds, Int) -> PData Int
lf =
  \ (lc :: Int#) (env :: PData Void) (arg :: PData (Preds, Int)) ->
    lf lc arg

vf :: Void -> (Preds, Int) -> Int
vf = \ (env :: Void) (arg :: (Preds, Int)) -> vf arg

lf :: Int# -> PData (Preds, Int) -> PData Int
lf =
  \ (lc :: Int#) (ds :: PData (Preds, Int)) ->
    let {
      scrut :: PData (Preds, Int)
      scrut = ds } in
    case scrut `cast` ... of wild { PTuple2 a b -> b }

vf :: (Preds, Int) -> Int
vf =
  \ (ds :: (Preds, Int)) ->
    let {
      scrut :: (Preds, Int)
      scrut = ds } in
    case scrut of wild { (a, b) -> b }

lminBoth
  :: Int#
     -> PData Void
     -> PData (V:GHC:PArr_[::] (Preds, Int))
     -> PData (Preds, Int)
lminBoth =
  \ (lc :: Int#)
    (env :: PData Void)
    (arg :: PData (V:GHC:PArr_[::] (Preds, Int))) ->
    lminBoth lc arg

vminBoth :: Void -> V:GHC:PArr_[::] (Preds, Int) -> (Preds, Int)
vminBoth =
  \ (env :: Void) (arg :: V:GHC:PArr_[::] (Preds, Int)) ->
    vminBoth arg

lminBoth
  :: Int#
     -> PData (V:GHC:PArr_[::] (Preds, Int)) -> PData (Preds, Int)
lminBoth =
  \ (lc :: Int#) (ds :: PData (V:GHC:PArr_[::] (Preds, Int))) ->
    liftedApply
      lc
      (replicatePD
         ($fPA:->
            (($fPR:->) `cast` ...)
            ($fPAPArray
               (($fPRPArray
                   ($p1PA
                      ($fPA(,)
                         (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                         $paPreds
                         $fPAInt)))
                `cast` ...)
               ($fPA(,)
                  (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
                  $paPreds
                  $fPAInt))
            ($fPA(,)
               (($fPR(,) ($fPRWrap $paPreds) ($fPRWrap $fPAInt)) `cast` ...)
               $paPreds
               $fPAInt))
         lc
         $vminBySndP)
      ds

vminBoth :: V:GHC:PArr_[::] (Preds, Int) -> (Preds, Int)
vminBoth =
  \ (ds :: V:GHC:PArr_[::] (Preds, Int)) -> $: $vminBySndP ds
end Rec }



