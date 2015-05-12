
-- Define the functions each implementation has to provide

module MyImplHistogramBalance (hbalance,hbalanceBulk) where

type Image a  -- the implementation type of Images can be chosed
type Many a   -- the implementation of the bulk of iamges xan be chosed

img :: Image Int -- let img be an example image

imgs :: Many (Image Int) -- let imgs be a collection of images for bulk operation

-- single application of HistogramBalance
hbalance :: Image Int -> Image Int

-- bulk application of HistrogramBalance
hbalanceBulk :: Many (Image Int) -> Many (Image Int)

