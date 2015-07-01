
import Control.Monad

main = csortM 5 [3,5,2,3,2,3,0,0]

-- war ein Versuch eines Alternativen Algorithmus
-- nichts schnell genug. Counting Sort hat kaum Parallelisierungsmöglichkeiten

csortM :: Int -> [Int] -> IO [Int]
csortM k xs =
  do  
      
      putStrLn $ "In: " ++ show xs
      let c1 = replicate (k+1) 0
      putStrLn $ "c1: " ++ show c1
      
      let c2 =
            foldr
              (\x c ->  (c ! x) << (c !! x) + 1)
              c1
            $ xs
      
      putStrLn $ "Hist c2 = " ++ show c2
      
      let c3 = tail . scanl (+) 0 $ c2
      
      putStrLn $ "Accu c3 = " ++ show c3
      
      (out,c4) <-
        foldM
          (\(out,c) x ->
            do  let idx = c !! x - 1
                    out' = (out !) idx << x
                    c' =  (c ! x) << idx
                putStrLn $ "out ! " ++ show idx ++ " = " ++ show x
                            ++ " ## (o,c) = " ++ show (out',c')
                return (out',c')
          )
          (replicate (length xs) (-1), c3)
        $ reverse xs
      
      putStrLn $ "Out out = " ++ show out
      putStrLn $ "Fin  c4 = " ++ show c4
      
      return out
      

infix 7 !
(!) :: a -> b -> (a,b)
(!) = (,)

infix 1 <<
(<<) :: ([a],Int) -> a -> [a]
([],_) << _ = []
((_:xs),0) << x = x:xs
((x:xs), n) << a = x:( xs ! (n - 1) << a)

