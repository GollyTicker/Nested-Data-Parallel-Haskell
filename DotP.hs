{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

-- The fully vectorized Module using Parallel Arrays

module DotP (dotp_wrapper) where
 
import qualified Prelude as P
import Data.Array.Parallel hiding ((*))
import Data.Array.Parallel.Prelude hiding ((*))
import Data.Array.Parallel.Prelude.Double
 
dotp_double :: [:Double:] -> [:Double:] -> Double
dotp_double xs ys = sumP [: x * y | x <- xs | y <- ys:]
 
dotp_wrapper :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotp_wrapper #-}
dotp_wrapper v w = dotp_double (fromPArrayP v) (fromPArrayP w)


