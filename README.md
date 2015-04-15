# Nested-Data-Parallel-Haskell
Nested Data Parallel Haskell - Stuff with DPH https://wiki.haskell.org/GHC/Data_Parallel_Haskell


Installation of NDPH (as worked on Ubuntu 14.04)
-----------------------------------

1. An installation of the Haskell platform is assumed. Note that you may receive warnings such as "Could not figure out LLVM version!"
2. Install LLVM 3.4
	- `sudo apt-get install clang-3.4 clang-3.4-doc libclang-common-3.4-dev libclang-3.4-dev libclang1-3.4 libclang1-3.4-dbg libllvm-3.4-ocaml-dev libllvm3.4 libllvm3.4-dbg lldb-3.4 llvm-3.4 llvm-3.4-dev llvm-3.4-doc llvm-3.4-examples llvm-3.4-runtime clang-modernize-3.4 clang-format-3.4 python-clang-3.4 lldb-3.4-dev` 
3. Check that llvm 3.4 was installed: The first line in `clang -v`  should contain LLVM 3.4
4. Add the current llvm into your path: e.g. `/usr/lib/llvm-3.4/bin` into `/etc/environment` 
5. Install DPH: `cabal install dph-examples`
5. Specify the used DPH interface: `ghc-pkg expose dph-lifted-vseg-0.7.0.1`


Compiling
-----------------------------------

1. Run `./build.sh`


Running
-----------------------------------

1. Run `./run.sh`



