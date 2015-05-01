#!/bin/bash


#  -dcore-lint    -ddump-to-file    -ddump-asm    -ddump-bcos   -ddump-cmm
#       -ddump-core-stats  -ddump-cpranal  -ddump-cse  -ddump-deriv   -ddump-ds
#       -ddump-flatC  -ddump-foreign  -ddump-hpc  -ddump-inlinings  -ddump-llvm
#       -ddump-occur-anal     -ddump-opt-cmm     -ddump-parsed      -ddump-prep
#       -ddump-rn    -ddump-rule-firings    -ddump-rule-rewrites   -ddump-rules
#       -ddump-vect  -ddump-simpl  -ddump-simpl-phases  -ddump-simpl-iterations
#       -ddump-spec    -ddump-splices   -ddump-stg   -ddump-stranal   -ddump-tc
#       -ddump-types  -ddump-worker-wrapper   -ddump-if-trace   -ddump-tc-trace
#       -ddump-vt-trace   -ddump-rn-trace   -ddump-rn-stats  -ddump-simpl-stats
#       -dno-debug-output    -dppr-debug    -dppr-noprags     -dppr-user-length
#       -dppr-colsNNN   -dppr-case-as-let   -dsuppress-all   -dsuppress-uniques
#       -dsuppress-idinfo                            -dsuppress-module-prefixes
#       -dsuppress-type-signatures                 -dsuppress-type-applications
#       -dsuppress-coercions     -dsource-stats      -dcmm-lint      -dstg-lint
#       -dstg-stats    -dverbose-core2core    -dverbose-stg2stg   -dshow-passes
#       -dfaststring-stats

     # -ddump-rule-firings  -ddump-vect \
     # -rtsopts -threaded -fcpr-off -fno-liberate-case
     
ghc -c \
     -Odph -package dph-lifted-vseg \
     -ddump-vect \
     -dsuppress-idinfo \
     -dsuppress-coercions -dsuppress-type-applications \
     -dsuppress-uniques -dsuppress-module-prefixes \
     "$1" -O4 Algo.hs > Algo.vect.hs
