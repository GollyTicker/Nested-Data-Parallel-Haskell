


sparseToDensePS :: Int -> a -> PA (Int,a) -> PA a
sparseTODensePS size z = 
  joinD
  . cmapD (\c ps' -> sparseToDenseS c size z ps')
  . splitSparseD size
  $ ps
  
{-

splitSparseD :: Int -> PA (Int,a) -> Dist (PA (Int,a)

"splitSparseD" splits a sparse vector into its distributed chunks
in such a way, that each gang member contains those elements of the sparse-vector
for which it were responsible for on a vector of length size.

sparseToDenseS :: Ctx -> Int -> a -> PA (Int,a) -> PA a
"sparseToDenseS".
Given a local chunk of a distributed sparse-vector,
the "sparseToDenseS" creates the corresponding dense vector.
The argument "c" is an argument which gives the context of the current gang
member, such that sprarseToDenseS can decide on the bounds of the local chunk to be created.

cmapD is a variant of mapD with additional contextual information about the
gang member running the mapping.(e.g. the indices pf the distributed array for which the current gang memer is responsible)
cmapD :: Ctx -> (a -> b) -> Dist a -> Dist b


-}

