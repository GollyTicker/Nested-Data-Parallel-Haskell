
import qualified Data.Map as M
import Data.List
import ListHistogramBalance (img)

-- tests that the algorithm used in the NDP and List variant are
-- functionally identical

main :: IO ()
main = do
  putStrLn "histL"
  print (histL img)
  putStrLn "histP"
  print (histP img)

type Image a = [[a]]
type Hist a = [(Int,a)]

histL :: Image Int -> Hist Int
histL = M.toList . foldr (\i -> M.insertWith (+) i 1) M.empty . concat


histP :: Image Int -> Hist Int
histP =
  map (\g -> (head g,length g))
  . group
  . sort
  . concat
