
import qualified Data.Vector.Unboxed as V
import Data.List (group,sort)
import Control.Monad.Par

gmax :: Int
gmax = 15

parAccuHist :: V.Vector Int -> V.Vector Int
parAccuHist img =
  case V.length img of
    0 -> replicate gmax 0   -- parallel
    
    1 ->  let x = img V.! 0
          in  generate gmax (\i -> if x <= i then 1 else 0) -- parallel
    
    len -> 
      let l = len `div` 2
          (left, right) = splitAt l img   -- O(1)
          (leftRes,rightRes) =

            runPar $ do
              -- create communication variables
              leftVar <- new
              rightVar <- new
              
              -- run left and right side in two "threads"
              fork $ put leftVar (parAccuHist left)
              fork $ put rightVar (parAccuHist right)
              
              -- get the results
              leftRes   <- get leftVar
              rightRes  <- get rightVar
              
              return (leftRes,rightRes)
        
        -- add them elementwise
      in  zipWith (+) leftRes rightRes -- parallel


main = print . runPar . parAccuHist $ img


img = V.fromList
      . concat
      $ [
           [1,1,1,1,1,1]
          ,[1,6,6,1,1,1]
          ,[5,5,5,0,1,0]
          ,[5,1,1,0,1,0]
          ,[6,6,1,1,1,1]
         ]
