


sparseToDensePS :: Int -> a -> PA (Int,a) -> PA a
sparseTODensePS size z = 
  joinD
  . sparseToDenseD size z
  . splitSparseD size
  $ ps
  
{-

sparseToDenseP :: Enum e => e -> a -> [: (e,a) :] -> [: a :]
sparseToDenseP              many init map            result
sparseToDenseP 8 0 [: (1,5),(2,4),(6,7) :] == [: 0,5,4,0,0,0,7,0 :]

sparseToDense n z map creates an array of length n where the element
at the index i has x if (i,x) is in the map, or z otherwise.
In effect it turns a sparse vector to a dense one

O(many)
Depth: 1
Work: many + lengthP map


splitSparseD :: Int -> PA (Int,a) -> Dist (PA (Int,a))

"splitSparseD" splits a sparse vector into its distributed chunks
in such a way, that each gang member contains those elements of the sparse-vector
for which it were responsible for on a vector of length size.

sparseToDenseD :: Int -> a -> Dist (PA (Int,a)) -> Dist (PA a)
Given a local chunk of a distributed sparse-vector,
the "sparseToDenseD" creates the corresponding local dense vector.

-}

