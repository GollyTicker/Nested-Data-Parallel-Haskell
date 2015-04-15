


{-
flatten [[1,2,3],[],[4],[5,6]] = ([1,2,3,4,5,6],[(0,3),(3,0),(3,1),(4,2)])

flatten :: [[a]] -> ([a],[(Int,Int)])
flatten xs = (concat xs, segd)
    where
        segd = undefined
-}


