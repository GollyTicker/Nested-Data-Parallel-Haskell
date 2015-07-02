

import Data.List (group,sort,nub,splitAt) -- for reference
import Test.QuickCheck
import Test.QuickCheck (Args(..))

as = [1,1,2,3,3,3,3,4]


{-

A local implementation of groupP.
Work(n) = O(n)
Depth(n) = O(log n)

How to build the segment descriptor:
1. distribute xs
2. locally build singleton-triples
3. in a tree-like recursive fasion ( see #23 )
   exchange the beginnings and ends of each chunk
   of the linked list and update them
4. transform the local linked-list-chunks into local segd array-chunks

-}


main = 
  do
    putStrLn "Input:"
    print as
    putStrLn "List group:"
    print $ group as
    putStrLn "Array groupP (internal flat view)"
    print $ groupP as
    putStrLn "Array groupP (external nested view)"
    print $ nestedToExplicit $ groupP as
    putStrLn "Testing with random data"
    quickCheckWith
      stdArgs { maxDiscardRatio = 500, maxSize = 6, maxSuccess = 1000 }
      prop_groupPworks

type AArr a = ([a],[(Int,Int)])

groupP :: Eq a => [a] -> ([a],[(Int,Int)]) -- indices and lengths
groupP as = (as, (convert . split 0) as)

split :: Eq a => Int -> [a] -> [(a,Int,Int)]
split i as =
  let l = length as
      dr = l `div` 2
      (left,right) = splitAt dr as
  in  case l of
        0 -> []
        1 -> [(head as,i,1)]
        _ -> merge (split i left) (split (i+dr) right)

-- [(Value,StartIdx,Count)]
merge :: Eq a => [(a,Int,Int)] -> [(a,Int,Int)] -> [(a,Int,Int)]
merge as bs
  | null as = bs
  | null bs = as
  | otherwise = 
      let (a,ast,ac) = last as
          (b,bst,bc) = head bs
      in  if a == b
            then init as ++ [(a,ast,ac + bc)] ++ tail bs
            else as ++ bs

convert :: [(a,Int,Int)] -> [(Int,Int)]
convert = map (\(a,i,m) -> (i,m))

-- from i m many.
sliceP :: Int -> Int -> [a] -> [a]
sliceP 0 m = take m 
sliceP n m = take m . tail

nestedToExplicit :: ([a],[(Int,Int)]) -> [[a]]
nestedToExplicit (as,ils) = map (\(i,l) -> (take l . drop i) as) ils

-- using Int instead of a, since it might degenerate to ()
-- prop_groupPworks :: [Int] -> Bool
prop_groupPworks xs = hasDups xs ==> group (xs :: [Int]) == (nestedToExplicit (groupP xs))

hasDups xs = length (nub xs) /= length xs

