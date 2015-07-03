



scanlP f z xs =
  joinD
  . mapD (\(as,a) -> mapS (f a) as)
  . propagateD f z
  . mapD (scanlS f z)
  . splitD
  $ xs

{-
       f          O(W)    O(D)
--------------------------------
  split           n       1
  scanlS          n       1
  propagate       n       log n
  mapS            n       1
  join            n       1
  scanlP          n       log n
-}
