


sparseToDensePS :: Int -> a -> PA (Int,a) -> PA a
sparseTODensePS size z = 
  joinD
  . sparseToDenseD size z
  . splitSparseD size
  $ ps
  
{-

splitSparseD :: Int -> PA (Int,a) -> Dist (PA (Int,a)

"splitSparseD" splits a sparse vector into its distributed chunks
in such a way, that each gang member contains those elements of the sparse-vector
for which it were responsible for on a vector of length size.

sparseToDenseD :: Int -> a -> Dist (PA (Int,a)) -> Dist (PA a)
Given a local chunk of a distributed sparse-vector,
the "sparseToDenseD" creates the corresponding local dense vector.

-}

