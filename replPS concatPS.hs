
"distribution transformation"
concatPS xs = joinD . cmapD (\ctx (DAArr from to n segd chunk) -> DAInt from to n chunk) . splitD
DAArr -> Distributed Array Array. The indices in the segd range from 0 to (from - to - 1). It's 
AAInt -> Distributed Array Int
Uses the context which is also transformed into a context suitable for an distributed int array
  DAInt :: from:Int -> to:Int -> totalLen:Int -> rawdata:(Vector Int) -> DAInt

replPS :: Int -> Int -> PA Int
replPS n x =
  joinD
  . cgenerateD (\ctx ->
                let (from, to) = arrayRange ctx n -- calculate which range we are responsible for
                in  DAInt from to n (replByteArray (from - to) x)
  )



