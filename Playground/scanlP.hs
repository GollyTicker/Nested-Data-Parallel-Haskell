

import Data.List (scanl) -- for reference
import Test.QuickCheck
import Test.QuickCheck (Args(..))
import Debug.Trace

as = [1..8]

test = scanlP 0 (+) 0 as

main = 
  do
    putStrLn "Input:"
    print as
    putStrLn "List scanl:"
    print $ scanl (+) 0 as
    putStrLn "Array scanl"
    print $ scanlP 0 (+) 0 as
    putStrLn "Testing with random data"
    quickCheckWith
      stdArgs { maxDiscardRatio = 500, maxSize = 6, maxSuccess = 1000 }
      prop_scanlPworks


evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x:odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

-- Belloch Programming Parallel Algorithms
scanlP :: Show a => Int -> (a -> a -> a) -> a -> [a] -> [a]
scanlP i f z xs =
  traceI i (show xs) $
    if null xs
      then traceI i "null" [z]
      else let e = traceI i "evens" $ evens xs
               o = traceI i "odds" $ odds xs
               fAdj = traceI i "fAdj" $ zipWith f e o
               s = traceI i "s" $ scanlP (i+1) f z fAdj
               span = traceI i "span" $ zipWith f s e
           in  traceI i "interleave" $ interleaveP s span

traceI :: Show a => Int -> String -> a -> a
traceI i s a = a -- trace (show i ++ ". " ++ s ++ ": " ++ show a) a

interleaveP :: [a] -> [a] -> [a]
interleaveP xs [] = xs
interleaveP [] ys = ys
interleaveP (x:xs) (y:ys) = x:y:interleaveP xs ys


prop_scanlPworks xs = scanl (+) 0 (xs :: [Int]) == (scanlP 0 (+) 0 xs)


