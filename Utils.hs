{-# LANGUAGE ParallelArrays #-}
-- {-# OPTIONS_GHC -fvectorise #-}
-- not vectorizing allows import from other modules

module Utils (
     reduceP
    ,reduce1P
    ,headP
    ,lastP
    ,tailP
    ,groupP
    ,sortP
    ,scanlP
    ,sparseToDenseP
    )
    where

import qualified Prelude as P
import Data.Array.Parallel hiding ((-))
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Prelude hiding ((-))
import Data.Array.Parallel.Prelude.Int

groupP :: Eq a => [:a:] -> [:[:a:]:]
groupP = groupP

sortP :: Ord a => [:a:] -> [:a:]
sortP = sortP

sparseToDenseP :: Int -> a -> [: (Int,a) :] -> [: a :]
sparseToDenseP = sparseToDenseP

scanlP :: (a -> b -> b) -> b -> [:a:] -> [:b:]
scanlP = scanlP

reduceP :: (a -> a -> a) -> a -> [:a:] -> a
reduceP = reduceP

reduce1P :: (a -> a -> a) -> [:a:] -> a
reduce1P f = \xs -> reduceP f (headP xs) (tailP xs)

headP :: [:a:] -> a
headP = (!: 0)

lastP :: [:a:] -> a
lastP xs = xs !: (lengthP xs - 1)

tailP :: [:a:] -> [:a:]
tailP xs = sliceP 1 (lengthP xs - 1) xs

-- sliceP :: i:Int -> lenInt -> [:a:] -> [:a:]
-- i: Index des ersten Elements
-- len: Länge des Slice
