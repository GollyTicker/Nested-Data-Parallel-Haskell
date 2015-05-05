#!/bin/bash

OPT="-O2"

ghc -c $OPT -Odph DotP.hs &&
ghc -c $OPT -Odph Main.hs &&
ghc -o dotp $OPT -threaded -rtsopts Main.hs

# LLVM?  ghc ... -fllvm ...

