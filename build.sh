#!/bin/bash

ghc -c -Odph \
    -ddump-vect -dsuppress-idinfo \
    -dsuppress-coercions -dsuppress-type-applications \
    -dsuppress-uniques -dsuppress-module-prefixes \
    DotP.hs &&
ghc -c -Odph Main.hs &&
ghc -o dotp -threaded -rtsopts Main.hs

# LLVM?  ghc ... -fllvm ...

