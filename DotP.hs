{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

-- The fully vectorized Module using Parallel Arrays

module DotP (dotp_wrapper) where
 
import qualified Prelude as P
import Data.Array.Parallel hiding ((*))
import Data.Array.Parallel.Prelude hiding ((*))
import Data.Array.Parallel.Prelude.Double
 
dotp :: [:Double:] -> [:Double:] -> Double
dotp xs ys = sumP [: x * y | x <- xs | y <- ys :]
 
dotpWrapper :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotpWrapper #-}
dotpWrapper v w = dotp (fromPArrayP v) (fromPArrayP w)


