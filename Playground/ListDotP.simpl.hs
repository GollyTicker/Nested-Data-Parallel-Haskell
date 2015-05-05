
==================== Tidy Core ====================
Result size of Tidy Core = {terms: 189, types: 85, coercions: 3}

lvl :: [Char]
lvl = unpackCString# "Negative exponent"

$s^1 :: Double
$s^1 = error lvl

lvl1 :: Integer
lvl1 = __integer 1

lvl2 :: Integer
lvl2 = __integer 2

Rec {
$wg1 :: Double# -> Integer -> Double# -> Double#
$wg1 =
  \ (ww :: Double#) (w :: Integer) (ww1 :: Double#) ->
    case eqInteger (remInteger w even2) even1 of _ {
      False ->
        case eqInteger w lvl1 of _ {
          False ->
            $wg1
              (*## ww ww) (quotInteger (minusInteger w lvl1) lvl2) (*## ww ww1);
          True -> *## ww ww1
        };
      True -> $wg1 (*## ww ww) (quotInteger w lvl2) ww1
    }
end Rec }

Rec {
$wf :: Double# -> Integer -> Double#
$wf =
  \ (ww :: Double#) (w :: Integer) ->
    case eqInteger (remInteger w even2) even1 of _ {
      False ->
        case eqInteger w lvl1 of _ {
          False ->
            $wg1 (*## ww ww) (quotInteger (minusInteger w lvl1) lvl2) ww;
          True -> ww
        };
      True -> $wf (*## ww ww) (quotInteger w lvl2)
    }
end Rec }

$s^2 :: Integer
$s^2 = __integer 0

$w$s^ :: Double -> Integer -> Double#
$w$s^ =
  \ (w :: Double) (w1 :: Integer) ->
    case ltInteger w1 $s^2 of _ {
      False ->
        case eqInteger w1 $s^2 of _ {
          False -> case w of _ { D# ww -> $wf ww w1 };
          True -> 1.0
        };
      True -> case $s^1 of wild1 { }
    }

$s^ :: Double -> Integer -> Double
$s^ =
  \ (w :: Double) (w1 :: Integer) ->
    case $w$s^ w w1 of ww { __DEFAULT -> D# ww }

Rec {
$wlgo :: Double# -> [Double] -> Double#
$wlgo =
  \ (ww :: Double#) (w :: [Double]) ->
    case w of _ {
      [] -> ww;
      : x xs -> case x of _ { D# y -> $wlgo (+## ww y) xs }
    }
end Rec }

main9 :: Double
main9 = D# 1.0

main7 :: Double
main7 = D# 2.0

main6 :: Integer
main6 = __integer 20

main11 :: Double
main11 = case $w$s^ main7 main6 of ww { __DEFAULT -> D# ww }

main10 :: [Double]
main10 =
  numericEnumFromTo $fOrdDouble $fFractionalDouble main9 main11

main8 :: Double
main8 = D# 3.0

main5 :: Double
main5 =
  case $w$s^ main7 main6 of ww { __DEFAULT -> D# (*## 3.0 ww) }

main4 :: [Double]
main4 =
  numericEnumFromThenTo
    $fOrdDouble $fFractionalDouble main9 main8 main5

main3 :: [Double]
main3 = zipWith timesDouble main10 main4

main2 :: String
main2 =
  case $wlgo 0.0 main3 of ww { __DEFAULT ->
  $w$sshowSignedFloat $fShowDouble_$sshowFloat shows26 ww ([])
  }

main1 :: State# RealWorld -> (# State# RealWorld, () #)
main1 =
  \ (eta :: State# RealWorld) -> hPutStr2 stdout main2 True eta

main :: IO ()
main = main1 `cast` ...


------ Local rules for imported ids --------
"SPEC ^ [Double, Integer]" [ALWAYS]
    forall ($dNum :: Num Double) ($dIntegral :: Integral Integer).
      ^ $dNum $dIntegral
      = $s^



==================== Cmm ====================
[section "data" { __stginit_main:ListDotP:
 }]



==================== Cmm ====================
[section "data" {
     lvl_closure:
         const lvl_info;
         const 0;
         const 0;
         const 0;
 },
 section "readonly" {
     cYI_str:
         I8[] [78,101,103,97,116,105,118,101,32,101,120,112,111,110,101,110,116]
 },
 lvl_info()
         { label: lvl_info
           rep:HeapRep static { Thunk }
         }
     cYL:
         if (Sp - 12 < SpLim) goto cYN;
         Hp = Hp + 8;
         if (Hp > HpLim) goto cYP;
         I32[Hp - 4] = stg_CAF_BLACKHOLE_info;
         I32[Hp + 0] = CurrentTSO;
         (_cYQ::I32,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 4, PtrHint));
         if (_cYQ::I32 == 0) goto cYR;
         goto cYS;
     cYN: jump stg_gc_enter_1; // [R1]
     cYP:
         HpAlloc = 8;
         goto cYN;
     cYR: jump I32[R1]; // [R1]
     cYS:
         I32[Sp - 8] = stg_bh_upd_frame_info;
         I32[Sp - 4] = Hp - 4;
         I32[Sp - 12] = cYI_str;
         Sp = Sp - 12;
         jump unpackCString#_info; // []
 }]



==================== Cmm ====================
[section "relreadonly" {
     $s^1_srt:
         const error_closure;
         const lvl_closure;
 },
 section "data" {
     $s^1_closure:
         const $s^1_info;
         const 0;
         const 0;
         const 0;
 },
 $s^1_info()
         { label: $s^1_info
           rep:HeapRep static { Thunk }
         }
     cZ9:
         if (Sp - 12 < SpLim) goto cZb;
         Hp = Hp + 8;
         if (Hp > HpLim) goto cZd;
         I32[Hp - 4] = stg_CAF_BLACKHOLE_info;
         I32[Hp + 0] = CurrentTSO;
         (_cZe::I32,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 4, PtrHint));
         if (_cZe::I32 == 0) goto cZf;
         goto cZg;
     cZb: jump stg_gc_enter_1; // [R1]
     cZd:
         HpAlloc = 8;
         goto cZb;
     cZf: jump I32[R1]; // [R1]
     cZg:
         I32[Sp - 8] = stg_bh_upd_frame_info;
         I32[Sp - 4] = Hp - 4;
         I32[Sp - 12] = lvl_closure;
         Sp = Sp - 12;
         jump error_info; // []
 }]



==================== Cmm ====================
[section "data" {
     lvl1_closure:
         const S#_static_info;
         const 1;
 }]



==================== Cmm ====================
[section "data" {
     lvl2_closure:
         const S#_static_info;
         const 2;
 }]



==================== Cmm ====================
[section "relreadonly" {
     $wg1_srt:
         const even1_closure;
         const even2_closure;
         const lvl1_closure;
         const lvl2_closure;
         const $wg1_closure;
 },
 section "data" {
     $wg1_closure:
         const $wg1_info;
         const 0;
 },
 $wg1_slow()
         {
         }
     c10c:
         Sp = Sp + 0;
         jump $wg1_info; // []
 },
 sZw_ret()
         { label: sZw_info
           rep:StackRep [True, True, True, True, True]
         }
     c10A:
         _sZx::F64 = %MO_F_Mul_W64(F64[Sp + 4], F64[Sp + 4]);
         I32[Sp + 12] = R1;
         F64[Sp + 4] = _sZx::F64;
         Sp = Sp + 4;
         jump $wg1_info; // []
 },
 sZu_ret()
         { label: sZu_info
           rep:StackRep [True, True, True, True, True]
         }
     c10C:
         I32[Sp - 4] = lvl2_closure+1;
         I32[Sp - 8] = R1;
         I32[Sp + 0] = sZw_info;
         Sp = Sp - 8;
         jump quotInteger_info; // []
 },
 sZz_ret()
         { label: sZz_info
           rep:StackRep [True, True, False, True, True]
         }
     c10G:
         _c10H::I32 = R1 & 3;
         if (_c10H::I32 >= 2) goto c10I;
         _sZv::F64 = %MO_F_Mul_W64(F64[Sp + 4], F64[Sp + 16]);
         F64[Sp + 16] = _sZv::F64;
         I32[Sp - 4] = lvl1_closure+1;
         I32[Sp - 8] = I32[Sp + 12];
         I32[Sp + 0] = sZu_info;
         Sp = Sp - 8;
         jump minusInteger_info; // []
     c10I:
         _c10M::F64 = %MO_F_Mul_W64(F64[Sp + 4], F64[Sp + 16]);
         D1 = _c10M::F64;
         Sp = Sp + 24;
         jump (I32[Sp + 0]); // [D1]
 },
 sZB_ret()
         { label: sZB_info
           rep:StackRep [True, True, True, True, True]
         }
     c10S:
         _sZC::F64 = %MO_F_Mul_W64(F64[Sp + 4], F64[Sp + 4]);
         I32[Sp + 12] = R1;
         F64[Sp + 4] = _sZC::F64;
         Sp = Sp + 4;
         jump $wg1_info; // []
 },
 sZA_ret()
         { label: sZA_info
           rep:StackRep [True, True, False, True, True]
         }
     c10W:
         _c10X::I32 = R1 & 3;
         if (_c10X::I32 >= 2) goto c10Y;
         I32[Sp - 4] = lvl1_closure+1;
         I32[Sp - 8] = I32[Sp + 12];
         I32[Sp + 0] = sZz_info;
         Sp = Sp - 8;
         jump eqInteger_info; // []
     c10Y:
         I32[Sp - 4] = lvl2_closure+1;
         I32[Sp - 8] = I32[Sp + 12];
         I32[Sp + 0] = sZB_info;
         Sp = Sp - 8;
         jump quotInteger_info; // []
 },
 sZy_ret()
         { label: sZy_info
           rep:StackRep [True, True, False, True, True]
         }
     c110:
         I32[Sp - 4] = even1_closure;
         I32[Sp - 8] = R1;
         I32[Sp + 0] = sZA_info;
         Sp = Sp - 8;
         jump eqInteger_info; // []
 },
 $wg1_info()
         { label: $wg1_info
           rep:HeapRep static {
                 Fun {arity: 3 fun_type: ArgGen [True, True, False, True, True]} }
         }
     c112:
         if (Sp - 12 < SpLim) goto c114;
         I32[Sp - 8] = even2_closure;
         I32[Sp - 12] = I32[Sp + 8];
         I32[Sp - 4] = sZy_info;
         Sp = Sp - 12;
         jump remInteger_info; // []
     c114:
         R1 = $wg1_closure;
         Sp = Sp + 0;
         jump stg_gc_fun; // [R1]
 }]



==================== Cmm ====================
[section "relreadonly" {
     $wf_srt:
         const $wf_closure;
         const even1_closure;
         const even2_closure;
         const lvl1_closure;
         const lvl2_closure;
         const $wg1_closure;
 },
 section "data" {
     $wf_closure:
         const $wf_info;
         const 0;
 },
 $wf_slow()
         {
         }
     c12c:
         Sp = Sp + 0;
         jump $wf_info; // []
 },
 s11B_ret()
         { label: s11B_info
           rep:StackRep [True, True, True]
         }
     c12y:
         _s11C::F64 = %MO_F_Mul_W64(F64[Sp + 4], F64[Sp + 4]);
         F64[Sp + 8] = F64[Sp + 4];
         I32[Sp + 4] = R1;
         F64[Sp - 4] = _s11C::F64;
         Sp = Sp - 4;
         jump $wg1_info; // []
 },
 s11A_ret()
         { label: s11A_info
           rep:StackRep [True, True, True]
         }
     c12A:
         I32[Sp - 4] = lvl2_closure+1;
         I32[Sp - 8] = R1;
         I32[Sp + 0] = s11B_info;
         Sp = Sp - 8;
         jump quotInteger_info; // []
 },
 s11E_ret()
         { label: s11E_info
           rep:StackRep [True, True, False]
         }
     c12E:
         _c12F::I32 = R1 & 3;
         if (_c12F::I32 >= 2) goto c12G;
         I32[Sp - 4] = lvl1_closure+1;
         I32[Sp - 8] = I32[Sp + 12];
         I32[Sp + 0] = s11A_info;
         Sp = Sp - 8;
         jump minusInteger_info; // []
     c12G:
         D1 = F64[Sp + 4];
         Sp = Sp + 16;
         jump (I32[Sp + 0]); // [D1]
 },
 s11G_ret()
         { label: s11G_info
           rep:StackRep [True, True, True]
         }
     c12M:
         _s11H::F64 = %MO_F_Mul_W64(F64[Sp + 4], F64[Sp + 4]);
         I32[Sp + 12] = R1;
         F64[Sp + 4] = _s11H::F64;
         Sp = Sp + 4;
         jump $wf_info; // []
 },
 s11F_ret()
         { label: s11F_info
           rep:StackRep [True, True, False]
         }
     c12Q:
         _c12R::I32 = R1 & 3;
         if (_c12R::I32 >= 2) goto c12S;
         I32[Sp - 4] = lvl1_closure+1;
         I32[Sp - 8] = I32[Sp + 12];
         I32[Sp + 0] = s11E_info;
         Sp = Sp - 8;
         jump eqInteger_info; // []
     c12S:
         I32[Sp - 4] = lvl2_closure+1;
         I32[Sp - 8] = I32[Sp + 12];
         I32[Sp + 0] = s11G_info;
         Sp = Sp - 8;
         jump quotInteger_info; // []
 },
 s11D_ret()
         { label: s11D_info
           rep:StackRep [True, True, False]
         }
     c12U:
         I32[Sp - 4] = even1_closure;
         I32[Sp - 8] = R1;
         I32[Sp + 0] = s11F_info;
         Sp = Sp - 8;
         jump eqInteger_info; // []
 },
 $wf_info()
         { label: $wf_info
           rep:HeapRep static {
                 Fun {arity: 2 fun_type: ArgGen [True, True, False]} }
         }
     c12W:
         if (Sp - 12 < SpLim) goto c12Y;
         I32[Sp - 8] = even2_closure;
         I32[Sp - 12] = I32[Sp + 8];
         I32[Sp - 4] = s11D_info;
         Sp = Sp - 12;
         jump remInteger_info; // []
     c12Y:
         R1 = $wf_closure;
         Sp = Sp + 0;
         jump stg_gc_fun; // [R1]
 }]



==================== Cmm ====================
[section "data" {
     $s^2_closure:
         const S#_static_info;
         const 0;
 }]



==================== Cmm ====================
[section "relreadonly" {
     $w$s^_srt:
         const $s^1_closure;
         const $wf_closure;
         const $s^2_closure;
 },
 section "data" {
     $w$s^_closure:
         const $w$s^_info;
         const 0;
 },
 s13r_ret()
         { label: s13r_info
           rep:StackRep [False]
         }
     c13V:
         F64[Sp - 4] = F64[R1 + 3];
         Sp = Sp - 4;
         jump $wf_info; // []
 },
 s13s_ret()
         { label: s13s_info
           rep:StackRep [False, False]
         }
     c13Z:
         _c140::I32 = R1 & 3;
         if (_c140::I32 >= 2) goto c141;
         R1 = I32[Sp + 4];
         I32[Sp + 4] = s13r_info;
         Sp = Sp + 4;
         if (R1 & 3 != 0) goto c144;
         jump I32[R1]; // [R1]
     c141:
         D1 = 1.0 :: W64;
         Sp = Sp + 12;
         jump (I32[Sp + 0]); // [D1]
     c144: jump s13r_info; // [R1]
 },
 s13t_ret()
         { label: s13t_info
           rep:StackRep [False, False]
         }
     c148:
         _c149::I32 = R1 & 3;
         if (_c149::I32 >= 2) goto c14a;
         I32[Sp - 4] = $s^2_closure+1;
         I32[Sp - 8] = I32[Sp + 8];
         I32[Sp + 0] = s13s_info;
         Sp = Sp - 8;
         jump eqInteger_info; // []
     c14a:
         R1 = $s^1_closure;
         Sp = Sp + 12;
         R1 = R1 & (-4);
         jump I32[R1]; // [R1]
 },
 $w$s^_info()
         { label: $w$s^_info
           rep:HeapRep static { Fun {arity: 2 fun_type: ArgSpec 12} }
         }
     c14c:
         if (Sp - 12 < SpLim) goto c14e;
         I32[Sp - 8] = $s^2_closure+1;
         I32[Sp - 12] = I32[Sp + 4];
         I32[Sp - 4] = s13t_info;
         Sp = Sp - 12;
         jump ltInteger_info; // []
     c14e:
         R1 = $w$s^_closure;
         jump stg_gc_fun; // [R1]
 }]



==================== Cmm ====================
[section "relreadonly" {
     $s^_srt:
         const $w$s^_closure;
 },
 section "data" {
     $s^_closure:
         const $s^_info;
         const 0;
 },
 sYe_ret()
         { label: sYe_info
           rep:StackRep []
         }
     c14G:
         Hp = Hp + 12;
         if (Hp > HpLim) goto c14M;
         I32[Hp - 8] = D#_con_info;
         F64[Hp - 4] = D1;
         R1 = Hp - 7;
         Sp = Sp + 4;
         jump (I32[Sp + 0]); // [R1]
     c14N: jump stg_gc_d1; // [D1]
     c14M:
         HpAlloc = 12;
         goto c14N;
 },
 $s^_info()
         { label: $s^_info
           rep:HeapRep static { Fun {arity: 2 fun_type: ArgSpec 12} }
         }
     c14P:
         if (Sp - 4 < SpLim) goto c14R;
         I32[Sp - 4] = I32[Sp + 0];
         I32[Sp + 0] = I32[Sp + 4];
         I32[Sp + 4] = sYe_info;
         Sp = Sp - 4;
         jump $w$s^_info; // []
     c14R:
         R1 = $s^_closure;
         jump stg_gc_fun; // [R1]
 }]



==================== Cmm ====================
[section "data" {
     $wlgo_closure:
         const $wlgo_info;
 },
 $wlgo_slow()
         {
         }
     c15j:
         Sp = Sp + 0;
         jump $wlgo_info; // []
 },
 s154_ret()
         { label: s154_info
           rep:StackRep [True, True, False]
         }
     c15u:
         _s153::F64 = %MO_F_Add_W64(F64[Sp + 4], F64[R1 + 3]);
         F64[Sp + 4] = _s153::F64;
         Sp = Sp + 4;
         jump $wlgo_info; // []
 },
 s152_ret()
         { label: s152_info
           rep:StackRep [True, True, True]
         }
     c15y:
         _c15z::I32 = R1 & 3;
         if (_c15z::I32 >= 2) goto c15A;
         D1 = F64[Sp + 4];
         Sp = Sp + 16;
         jump (I32[Sp + 0]); // [D1]
     c15A:
         I32[Sp + 12] = I32[R1 + 6];
         R1 = I32[R1 + 2];
         I32[Sp + 0] = s154_info;
         if (R1 & 3 != 0) goto c15D;
         jump I32[R1]; // [R1]
     c15D: jump s154_info; // [R1]
 },
 $wlgo_info()
         { label: $wlgo_info
           rep:HeapRep static {
                 Fun {arity: 2 fun_type: ArgGen [True, True, False]} }
         }
     c15F:
         if (Sp - 4 < SpLim) goto c15H;
         R1 = I32[Sp + 8];
         I32[Sp - 4] = s152_info;
         Sp = Sp - 4;
         if (R1 & 3 != 0) goto c15K;
         jump I32[R1]; // [R1]
     c15H:
         R1 = $wlgo_closure;
         Sp = Sp + 0;
         jump stg_gc_fun; // [R1]
     c15K: jump s152_info; // [R1]
 }]



==================== Cmm ====================
[section "data" {
     main9_closure:
         const D#_static_info;
         const 1.0 :: W64;
 }]



==================== Cmm ====================
[section "data" {
     main7_closure:
         const D#_static_info;
         const 2.0 :: W64;
 }]



==================== Cmm ====================
[section "data" {
     main6_closure:
         const S#_static_info;
         const 20;
 }]



==================== Cmm ====================
[section "relreadonly" {
     main11_srt:
         const main6_closure;
         const $w$s^_closure;
 },
 section "data" {
     main11_closure:
         const main11_info;
         const 0;
         const 0;
         const 0;
 },
 sYt_ret()
         { label: sYt_info
           rep:StackRep []
         }
     c16k:
         Hp = Hp + 12;
         if (Hp > HpLim) goto c16q;
         I32[Hp - 8] = D#_con_info;
         F64[Hp - 4] = D1;
         R1 = Hp - 7;
         Sp = Sp + 4;
         jump (I32[Sp + 0]); // [R1]
     c16r: jump stg_gc_d1; // [D1]
     c16q:
         HpAlloc = 12;
         goto c16r;
 },
 main11_info()
         { label: main11_info
           rep:HeapRep static { Thunk }
         }
     c16u:
         if (Sp - 20 < SpLim) goto c16w;
         Hp = Hp + 8;
         if (Hp > HpLim) goto c16y;
         I32[Hp - 4] = stg_CAF_BLACKHOLE_info;
         I32[Hp + 0] = CurrentTSO;
         (_c16z::I32,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 4, PtrHint));
         if (_c16z::I32 == 0) goto c16A;
         goto c16B;
     c16w: jump stg_gc_enter_1; // [R1]
     c16y:
         HpAlloc = 8;
         goto c16w;
     c16A: jump I32[R1]; // [R1]
     c16B:
         I32[Sp - 8] = stg_bh_upd_frame_info;
         I32[Sp - 4] = Hp - 4;
         I32[Sp - 16] = main6_closure+1;
         I32[Sp - 20] = main7_closure+1;
         I32[Sp - 12] = sYt_info;
         Sp = Sp - 20;
         jump $w$s^_info; // []
 }]



==================== Cmm ====================
[section "relreadonly" {
     main10_srt:
         const main11_closure;
         const $fFractionalDouble_closure;
         const numericEnumFromTo_closure;
 },
 section "data" {
     main10_closure:
         const main10_info;
         const 0;
         const 0;
         const 0;
 },
 main10_info()
         { label: main10_info
           rep:HeapRep static { Thunk }
         }
     c16U:
         if (Sp - 24 < SpLim) goto c16W;
         Hp = Hp + 8;
         if (Hp > HpLim) goto c16Y;
         I32[Hp - 4] = stg_CAF_BLACKHOLE_info;
         I32[Hp + 0] = CurrentTSO;
         (_c16Z::I32,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 4, PtrHint));
         if (_c16Z::I32 == 0) goto c170;
         goto c171;
     c16W: jump stg_gc_enter_1; // [R1]
     c16Y:
         HpAlloc = 8;
         goto c16W;
     c170: jump I32[R1]; // [R1]
     c171:
         I32[Sp - 8] = stg_bh_upd_frame_info;
         I32[Sp - 4] = Hp - 4;
         I32[Sp - 12] = main11_closure;
         I32[Sp - 16] = main9_closure+1;
         I32[Sp - 20] = $fFractionalDouble_closure;
         I32[Sp - 24] = $fOrdDouble_closure;
         Sp = Sp - 24;
         jump numericEnumFromTo_info; // []
 }]



==================== Cmm ====================
[section "data" {
     main8_closure:
         const D#_static_info;
         const 3.0 :: W64;
 }]



==================== Cmm ====================
[section "relreadonly" {
     main5_srt:
         const main6_closure;
         const $w$s^_closure;
 },
 section "data" {
     main5_closure:
         const main5_info;
         const 0;
         const 0;
         const 0;
 },
 sYv_ret()
         { label: sYv_info
           rep:StackRep []
         }
     c17t:
         Hp = Hp + 12;
         if (Hp > HpLim) goto c17z;
         _s17c::F64 = %MO_F_Mul_W64(3.0 :: W64, D1);
         I32[Hp - 8] = D#_con_info;
         F64[Hp - 4] = _s17c::F64;
         R1 = Hp - 7;
         Sp = Sp + 4;
         jump (I32[Sp + 0]); // [R1]
     c17A: jump stg_gc_d1; // [D1]
     c17z:
         HpAlloc = 12;
         goto c17A;
 },
 main5_info()
         { label: main5_info
           rep:HeapRep static { Thunk }
         }
     c17D:
         if (Sp - 20 < SpLim) goto c17F;
         Hp = Hp + 8;
         if (Hp > HpLim) goto c17H;
         I32[Hp - 4] = stg_CAF_BLACKHOLE_info;
         I32[Hp + 0] = CurrentTSO;
         (_c17I::I32,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 4, PtrHint));
         if (_c17I::I32 == 0) goto c17J;
         goto c17K;
     c17F: jump stg_gc_enter_1; // [R1]
     c17H:
         HpAlloc = 8;
         goto c17F;
     c17J: jump I32[R1]; // [R1]
     c17K:
         I32[Sp - 8] = stg_bh_upd_frame_info;
         I32[Sp - 4] = Hp - 4;
         I32[Sp - 16] = main6_closure+1;
         I32[Sp - 20] = main7_closure+1;
         I32[Sp - 12] = sYv_info;
         Sp = Sp - 20;
         jump $w$s^_info; // []
 }]



==================== Cmm ====================
[section "relreadonly" {
     main4_srt:
         const main5_closure;
         const numericEnumFromThenTo_closure;
         const $fFractionalDouble_closure;
 },
 section "data" {
     main4_closure:
         const main4_info;
         const 0;
         const 0;
         const 0;
 },
 main4_info()
         { label: main4_info
           rep:HeapRep static { Thunk }
         }
     c185:
         if (Sp - 28 < SpLim) goto c187;
         Hp = Hp + 8;
         if (Hp > HpLim) goto c189;
         I32[Hp - 4] = stg_CAF_BLACKHOLE_info;
         I32[Hp + 0] = CurrentTSO;
         (_c18a::I32,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 4, PtrHint));
         if (_c18a::I32 == 0) goto c18b;
         goto c18c;
     c187: jump stg_gc_enter_1; // [R1]
     c189:
         HpAlloc = 8;
         goto c187;
     c18b: jump I32[R1]; // [R1]
     c18c:
         I32[Sp - 8] = stg_bh_upd_frame_info;
         I32[Sp - 4] = Hp - 4;
         I32[Sp - 12] = main5_closure;
         I32[Sp - 16] = main8_closure+1;
         I32[Sp - 20] = main9_closure+1;
         I32[Sp - 24] = $fFractionalDouble_closure;
         I32[Sp - 28] = $fOrdDouble_closure;
         Sp = Sp - 28;
         jump numericEnumFromThenTo_info; // []
 }]



==================== Cmm ====================
[section "relreadonly" {
     main3_srt:
         const main4_closure;
         const main10_closure;
 },
 section "data" {
     main3_closure:
         const main3_info;
         const 0;
         const 0;
         const 0;
 },
 main3_info()
         { label: main3_info
           rep:HeapRep static { Thunk }
         }
     c18t:
         if (Sp - 20 < SpLim) goto c18v;
         Hp = Hp + 8;
         if (Hp > HpLim) goto c18x;
         I32[Hp - 4] = stg_CAF_BLACKHOLE_info;
         I32[Hp + 0] = CurrentTSO;
         (_c18y::I32,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 4, PtrHint));
         if (_c18y::I32 == 0) goto c18z;
         goto c18A;
     c18v: jump stg_gc_enter_1; // [R1]
     c18x:
         HpAlloc = 8;
         goto c18v;
     c18z: jump I32[R1]; // [R1]
     c18A:
         I32[Sp - 8] = stg_bh_upd_frame_info;
         I32[Sp - 4] = Hp - 4;
         I32[Sp - 12] = main4_closure;
         I32[Sp - 16] = main10_closure;
         I32[Sp - 20] = timesDouble_closure+2;
         Sp = Sp - 20;
         jump zipWith_info; // []
 }]



==================== Cmm ====================
[section "relreadonly" {
     main2_srt:
         const $fShowDouble_$sshowFloat_closure;
         const main3_closure;
 },
 section "data" {
     main2_closure:
         const main2_info;
         const 0;
         const 0;
         const 0;
 },
 sYy_ret()
         { label: sYy_info
           rep:StackRep []
         }
     c18V:
         I32[Sp + 0] = []_closure+1;
         I32[Sp - 4] = stg_ap_p_info;
         F64[Sp - 12] = D1;
         I32[Sp - 16] = shows26_closure;
         I32[Sp - 20] = $fShowDouble_$sshowFloat_closure+1;
         Sp = Sp - 20;
         jump $w$sshowSignedFloat_info; // []
 },
 main2_info()
         { label: main2_info
           rep:HeapRep static { Thunk }
         }
     c18Y:
         if (Sp - 32 < SpLim) goto c190;
         Hp = Hp + 8;
         if (Hp > HpLim) goto c192;
         I32[Hp - 4] = stg_CAF_BLACKHOLE_info;
         I32[Hp + 0] = CurrentTSO;
         (_c193::I32,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 4, PtrHint));
         if (_c193::I32 == 0) goto c194;
         goto c195;
     c190: jump stg_gc_enter_1; // [R1]
     c192:
         HpAlloc = 8;
         goto c190;
     c194: jump I32[R1]; // [R1]
     c195:
         I32[Sp - 8] = stg_bh_upd_frame_info;
         I32[Sp - 4] = Hp - 4;
         I32[Sp - 16] = main3_closure;
         F64[Sp - 24] = 0.0 :: W64;
         I32[Sp - 12] = sYy_info;
         Sp = Sp - 24;
         jump $wlgo_info; // []
 }]



==================== Cmm ====================
[section "relreadonly" {
     main1_srt:
         const main2_closure;
         const stdout_closure;
         const hPutStr2_closure;
 },
 section "data" {
     main1_closure:
         const main1_info;
         const 0;
 },
 main1_info()
         { label: main1_info
           rep:HeapRep static { Fun {arity: 1 fun_type: ArgSpec 3} }
         }
     c19l:
         if (Sp - 12 < SpLim) goto c19o;
         I32[Sp - 4] = True_closure+2;
         I32[Sp - 8] = main2_closure;
         I32[Sp - 12] = stdout_closure;
         Sp = Sp - 12;
         jump hPutStr2_info; // []
     c19o:
         R1 = main1_closure;
         jump stg_gc_fun; // [R1]
 }]



==================== Cmm ====================
[section "relreadonly" {
     main_srt:
         const main1_closure;
 },
 section "data" {
     main_closure:
         const main_info;
         const 0;
 },
 main_info()
         { label: main_info
           rep:HeapRep static { Fun {arity: 1 fun_type: ArgSpec 3} }
         }
     c19y: jump main1_info; // []
 }]


