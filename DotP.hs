{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

-- The fully vectorized Module using Parallel Arrays

module DotP (
        example,
        dotp,
        smvm
    ) where
 
import qualified Prelude as P
import Data.Array.Parallel hiding ((*))
import Data.Array.Parallel.Prelude hiding ((*))
import Data.Array.Parallel.Prelude.Double

type Vector = [:Double:]
type SparseVector = [:(Double,Int):]
type SparseMatrix = [:SparseVector:]

-- tiny vectorized code. huge vectorized core!
dotp :: SparseVector -> Vector -> Double
dotp sv v = sumP [: d * (v !: i) | (d,i) <- sv :]

smvm :: SparseMatrix -> Vector -> Vector
smvm sm v = [: dotp sv v | sv <- sm :]

sm :: SparseMatrix
sm = [:
        [: (3.1,1), (3.2,2),(2.1,5) :],
        [: (5.5,0), (1.4,2) :],
        [: :],
        [: (21.1,0), (31.2,1),(1.1,3),(2.1,4) :]
     :]

v :: Vector
v = [: 2.4, 4.2, 1.2, 2.3, 6.5, 0.3 :]

example :: Double
example = sumP (smvm sm v)

