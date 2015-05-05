{-# LANGUAGE BangPatterns #-}

module ListDotP (main) where

import Data.List

xs = [1..2^20]
ys = [1,3..3*2^20]

main = print $ first xs ys
    --(first xs ys, second xs ys, third xs ys)
    
first :: [Double] -> [Double] -> Double
first xs = foldl' (+) 0 . zipWith (*) xs

second :: [Double] -> [Double] -> Double
second xs ys = foldr (+) 0 $ zipWith (*) xs ys

third :: [Double] -> [Double] -> Double
third xs ys = sum $ zipWith (*) xs ys



