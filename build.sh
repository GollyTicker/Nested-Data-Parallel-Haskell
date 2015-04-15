#!/bin/bash

ghc -c -Odph DotP.hs &&
ghc -c -Odph Main.hs &&
ghc -o dotp -threaded -rtsopts Main.hs

# LLVM?  ghc ... -fllvm ...

